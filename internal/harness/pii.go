package harness

import (
	"crypto/sha256"
	"encoding/hex"
	"regexp"
	"strings"
)

// PIIType categorizes types of personally identifiable information.
type PIIType string

const (
	PIITypeSSN         PIIType = "ssn"
	PIITypeCreditCard  PIIType = "credit_card"
	PIITypeEmail       PIIType = "email"
	PIITypePhone       PIIType = "phone"
	PIITypeIPAddress   PIIType = "ip_address"
	PIITypeDateOfBirth PIIType = "date_of_birth"
	PIITypeName        PIIType = "name"
	PIITypeAddress     PIIType = "address"
	PIITypeAccountNum  PIIType = "account_number"
)

// PIIMatch represents a detected PII instance.
type PIIMatch struct {
	Type     PIIType `json:"type"`
	Value    string  `json:"value"`
	Masked   string  `json:"masked"`
	StartPos int     `json:"start_pos"`
	EndPos   int     `json:"end_pos"`
	Field    string  `json:"field,omitempty"`
}

// PIIDetectorConfig configures PII detection behavior.
type PIIDetectorConfig struct {
	// EnabledTypes specifies which PII types to detect (empty = all)
	EnabledTypes []PIIType
	// MaskingStyle determines how to mask PII
	MaskingStyle MaskingStyle
	// PreserveLength keeps masked value same length as original
	PreserveLength bool
	// CustomPatterns allows adding custom regex patterns
	CustomPatterns map[string]*regexp.Regexp
}

// MaskingStyle determines how PII is masked.
type MaskingStyle string

const (
	MaskingStyleRedact   MaskingStyle = "redact"   // Replace with [REDACTED]
	MaskingStylePartial  MaskingStyle = "partial"  // Show first/last chars
	MaskingStyleHash     MaskingStyle = "hash"     // Replace with hash
	MaskingStyleTokenize MaskingStyle = "tokenize" // Replace with token
)

// DefaultPIIDetectorConfig returns default configuration.
func DefaultPIIDetectorConfig() *PIIDetectorConfig {
	return &PIIDetectorConfig{
		EnabledTypes:   nil, // All types
		MaskingStyle:   MaskingStyleRedact,
		PreserveLength: false,
	}
}

// PIIDetector detects and masks PII in text and structured data.
type PIIDetector struct {
	config   *PIIDetectorConfig
	patterns map[PIIType]*regexp.Regexp
}

// NewPIIDetector creates a new PII detector.
func NewPIIDetector(config *PIIDetectorConfig) *PIIDetector {
	if config == nil {
		config = DefaultPIIDetectorConfig()
	}

	d := &PIIDetector{
		config:   config,
		patterns: make(map[PIIType]*regexp.Regexp),
	}

	// Initialize built-in patterns
	d.patterns[PIITypeSSN] = regexp.MustCompile(`\b\d{3}-\d{2}-\d{4}\b`)
	d.patterns[PIITypeCreditCard] = regexp.MustCompile(`\b(?:\d{4}[-\s]?){3}\d{4}\b`)
	d.patterns[PIITypeEmail] = regexp.MustCompile(`\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b`)
	d.patterns[PIITypePhone] = regexp.MustCompile(`\b(?:\+?1[-.\s]?)?\(?\d{3}\)?[-.\s]?\d{3}[-.\s]?\d{4}\b`)
	d.patterns[PIITypeIPAddress] = regexp.MustCompile(`\b(?:\d{1,3}\.){3}\d{1,3}\b`)
	d.patterns[PIITypeDateOfBirth] = regexp.MustCompile(`\b(?:0?[1-9]|1[0-2])[/-](?:0?[1-9]|[12]\d|3[01])[/-](?:19|20)\d{2}\b`)
	d.patterns[PIITypeAccountNum] = regexp.MustCompile(`\b[A-Z]{0,3}\d{8,17}\b`)

	// Add custom patterns
	for name, pattern := range config.CustomPatterns {
		d.patterns[PIIType(name)] = pattern
	}

	return d
}

// DetectInText scans text for PII and returns all matches.
func (d *PIIDetector) DetectInText(text string) []PIIMatch {
	var matches []PIIMatch

	for piiType, pattern := range d.patterns {
		if !d.isTypeEnabled(piiType) {
			continue
		}

		locs := pattern.FindAllStringIndex(text, -1)
		for _, loc := range locs {
			value := text[loc[0]:loc[1]]
			matches = append(matches, PIIMatch{
				Type:     piiType,
				Value:    value,
				Masked:   d.mask(value, piiType),
				StartPos: loc[0],
				EndPos:   loc[1],
			})
		}
	}

	return matches
}

// MaskText replaces all PII in text with masked values.
func (d *PIIDetector) MaskText(text string) (string, []PIIMatch) {
	matches := d.DetectInText(text)
	if len(matches) == 0 {
		return text, nil
	}

	// Sort by position descending to replace from end to start
	// This preserves positions during replacement
	sortedMatches := make([]PIIMatch, len(matches))
	copy(sortedMatches, matches)
	for i := 0; i < len(sortedMatches)-1; i++ {
		for j := i + 1; j < len(sortedMatches); j++ {
			if sortedMatches[j].StartPos > sortedMatches[i].StartPos {
				sortedMatches[i], sortedMatches[j] = sortedMatches[j], sortedMatches[i]
			}
		}
	}

	result := text
	for _, match := range sortedMatches {
		result = result[:match.StartPos] + match.Masked + result[match.EndPos:]
	}

	return result, matches
}

// DetectInMap scans a map for PII values.
func (d *PIIDetector) DetectInMap(data map[string]any) []PIIMatch {
	var matches []PIIMatch
	d.detectInValue("", data, &matches)
	return matches
}

// MaskMap replaces PII in a map with masked values.
func (d *PIIDetector) MaskMap(data map[string]any) (map[string]any, []PIIMatch) {
	var matches []PIIMatch
	result := d.maskValue("", data, &matches)
	if resultMap, ok := result.(map[string]any); ok {
		return resultMap, matches
	}
	return data, matches
}

func (d *PIIDetector) detectInValue(field string, value any, matches *[]PIIMatch) {
	switch v := value.(type) {
	case string:
		for _, match := range d.DetectInText(v) {
			match.Field = field
			*matches = append(*matches, match)
		}
	case map[string]any:
		for k, val := range v {
			fieldPath := k
			if field != "" {
				fieldPath = field + "." + k
			}
			d.detectInValue(fieldPath, val, matches)
		}
	case []any:
		for i, item := range v {
			fieldPath := field
			if field != "" {
				fieldPath = field + "[" + string(rune('0'+i)) + "]"
			}
			d.detectInValue(fieldPath, item, matches)
		}
	}
}

func (d *PIIDetector) maskValue(field string, value any, matches *[]PIIMatch) any {
	switch v := value.(type) {
	case string:
		masked, fieldMatches := d.MaskText(v)
		for i := range fieldMatches {
			fieldMatches[i].Field = field
		}
		*matches = append(*matches, fieldMatches...)
		return masked
	case map[string]any:
		result := make(map[string]any)
		for k, val := range v {
			fieldPath := k
			if field != "" {
				fieldPath = field + "." + k
			}
			result[k] = d.maskValue(fieldPath, val, matches)
		}
		return result
	case []any:
		result := make([]any, len(v))
		for i, item := range v {
			fieldPath := field
			if field != "" {
				fieldPath = field + "[" + string(rune('0'+i)) + "]"
			}
			result[i] = d.maskValue(fieldPath, item, matches)
		}
		return result
	default:
		return value
	}
}

func (d *PIIDetector) isTypeEnabled(piiType PIIType) bool {
	if len(d.config.EnabledTypes) == 0 {
		return true
	}
	for _, t := range d.config.EnabledTypes {
		if t == piiType {
			return true
		}
	}
	return false
}

func (d *PIIDetector) mask(value string, piiType PIIType) string {
	switch d.config.MaskingStyle {
	case MaskingStyleRedact:
		return d.redact(value, piiType)
	case MaskingStylePartial:
		return d.partialMask(value, piiType)
	case MaskingStyleHash:
		return d.hashMask(value)
	case MaskingStyleTokenize:
		return d.tokenize(value, piiType)
	default:
		return d.redact(value, piiType)
	}
}

func (d *PIIDetector) redact(value string, piiType PIIType) string {
	if d.config.PreserveLength {
		return strings.Repeat("*", len(value))
	}
	return "[REDACTED:" + string(piiType) + "]"
}

func (d *PIIDetector) partialMask(value string, piiType PIIType) string {
	if len(value) <= 4 {
		return strings.Repeat("*", len(value))
	}

	switch piiType {
	case PIITypeCreditCard:
		// Show last 4 digits
		clean := strings.ReplaceAll(strings.ReplaceAll(value, "-", ""), " ", "")
		if len(clean) >= 4 {
			return "****-****-****-" + clean[len(clean)-4:]
		}
	case PIITypeSSN:
		// Show last 4 digits
		return "***-**-" + value[len(value)-4:]
	case PIITypeEmail:
		// Show first char and domain
		parts := strings.Split(value, "@")
		if len(parts) == 2 {
			return string(parts[0][0]) + "***@" + parts[1]
		}
	case PIITypePhone:
		// Show last 4 digits
		clean := regexp.MustCompile(`\D`).ReplaceAllString(value, "")
		if len(clean) >= 4 {
			return "(***) ***-" + clean[len(clean)-4:]
		}
	}

	// Default: show first and last char
	return string(value[0]) + strings.Repeat("*", len(value)-2) + string(value[len(value)-1])
}

func (d *PIIDetector) hashMask(value string) string {
	hash := sha256.Sum256([]byte(value))
	return "HASH:" + hex.EncodeToString(hash[:8])
}

func (d *PIIDetector) tokenize(value string, piiType PIIType) string {
	hash := sha256.Sum256([]byte(value))
	return "TOKEN:" + string(piiType) + ":" + hex.EncodeToString(hash[:4])
}

// MaskFixture masks PII in a fixture's HTTP body and expected body.
func (d *PIIDetector) MaskFixture(fixture *Fixture) (*Fixture, []PIIMatch) {
	var allMatches []PIIMatch
	result := *fixture // Copy

	if fixture.HTTP != nil {
		httpCopy := *fixture.HTTP
		result.HTTP = &httpCopy

		// Mask request body
		if len(fixture.HTTP.Body) > 0 {
			masked, matches := d.MaskText(string(fixture.HTTP.Body))
			result.HTTP.Body = []byte(masked)
			allMatches = append(allMatches, matches...)
		}

		// Mask expected body
		if len(fixture.HTTP.ExpectedBody) > 0 {
			masked, matches := d.MaskText(string(fixture.HTTP.ExpectedBody))
			result.HTTP.ExpectedBody = []byte(masked)
			allMatches = append(allMatches, matches...)
		}
	}

	return &result, allMatches
}

// MaskDBSnapshot masks PII in a database snapshot.
func (d *PIIDetector) MaskDBSnapshot(snapshot *DBSnapshot) (*DBSnapshot, []PIIMatch) {
	var allMatches []PIIMatch
	result := &DBSnapshot{
		Timestamp: snapshot.Timestamp,
		Hash:      "", // Will be recalculated
		Tables:    make(map[string]*TableSnapshot),
	}

	for tableName, table := range snapshot.Tables {
		maskedTable := &TableSnapshot{
			Name:     table.Name,
			RowCount: table.RowCount,
			Columns:  table.Columns,
			Rows:     make(map[string]map[string]any),
		}

		for rowKey, row := range table.Rows {
			maskedRow, matches := d.MaskMap(row)
			maskedTable.Rows[rowKey] = maskedRow
			for i := range matches {
				matches[i].Field = tableName + "." + matches[i].Field
			}
			allMatches = append(allMatches, matches...)
		}

		result.Tables[tableName] = maskedTable
	}

	return result, allMatches
}
