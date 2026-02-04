package harness

import (
	"strings"
	"testing"
	"time"
)

// ==================== Fixtures Tests ====================

func TestReadJSONL(t *testing.T) {
	in := `{"kind":"http","name":"bill_inquiry","http":{"method":"POST","path":"/bill/inquiry","expected_status":200,"expected_body":{"ok":true}}}
{"kind":"batch","name":"daily_recon","batch":{"inputs":["in.dat"],"outputs":["out.dat"]}}
`
	fixtures, err := ReadJSONL(strings.NewReader(in))
	if err != nil {
		t.Fatal(err)
	}
	if len(fixtures) != 2 {
		t.Fatalf("expected 2 fixtures, got %d", len(fixtures))
	}
}

func TestReadJSONL_EmptyLines(t *testing.T) {
	in := `{"kind":"http","name":"test1","http":{"method":"GET","path":"/test","expected_status":200}}

{"kind":"http","name":"test2","http":{"method":"GET","path":"/test2","expected_status":200}}
`
	fixtures, err := ReadJSONL(strings.NewReader(in))
	if err != nil {
		t.Fatal(err)
	}
	if len(fixtures) != 2 {
		t.Fatalf("expected 2 fixtures, got %d", len(fixtures))
	}
}

func TestReadJSONL_MissingKind(t *testing.T) {
	in := `{"name":"test1","http":{"method":"GET","path":"/test","expected_status":200}}`
	_, err := ReadJSONL(strings.NewReader(in))
	if err == nil {
		t.Fatal("expected error for missing kind")
	}
	if !strings.Contains(err.Error(), "missing kind") {
		t.Fatalf("expected 'missing kind' error, got: %v", err)
	}
}

func TestReadJSONL_MissingName(t *testing.T) {
	in := `{"kind":"http","http":{"method":"GET","path":"/test","expected_status":200}}`
	_, err := ReadJSONL(strings.NewReader(in))
	if err == nil {
		t.Fatal("expected error for missing name")
	}
	if !strings.Contains(err.Error(), "missing name") {
		t.Fatalf("expected 'missing name' error, got: %v", err)
	}
}

func TestReadJSONL_InvalidJSON(t *testing.T) {
	in := `{"kind":"http","name":"test1",invalid}`
	_, err := ReadJSONL(strings.NewReader(in))
	if err == nil {
		t.Fatal("expected error for invalid JSON")
	}
}

// ==================== DiffHTTP Tests ====================

func TestDiffHTTP_NormalizeTopLevelField(t *testing.T) {
	f := Fixture{
		Kind: "http",
		Name: "x",
		HTTP: &HTTPFixture{
			Method:         "GET",
			Path:           "/x",
			ExpectedStatus: 200,
			ExpectedBody:   []byte(`{"ok":true,"ts":"ignore-me"}`),
		},
		Normalize: &NormalizeRules{IgnoreJSONFields: []string{"ts"}},
	}

	res := DiffHTTP(f, HTTPActual{
		Status: 200,
		Body:   []byte(`{"ts":"different","ok":true}`),
	}, nil)

	if !res.Pass {
		t.Fatalf("expected pass, got: %+v", res)
	}
}

func TestDiffHTTP_StatusMismatch(t *testing.T) {
	f := Fixture{
		Kind: FixtureHTTP,
		Name: "status_test",
		HTTP: &HTTPFixture{
			ExpectedStatus: 200,
		},
	}

	res := DiffHTTP(f, HTTPActual{Status: 404}, nil)
	if res.Pass {
		t.Fatal("expected fail for status mismatch")
	}
	if !strings.Contains(res.Reason, "status mismatch") {
		t.Fatalf("expected status mismatch reason, got: %s", res.Reason)
	}
}

func TestDiffHTTP_HeaderMismatch(t *testing.T) {
	f := Fixture{
		Kind: FixtureHTTP,
		Name: "header_test",
		HTTP: &HTTPFixture{
			ExpectedStatus: 200,
			ExpectedHeader: map[string]string{"Content-Type": "application/json"},
		},
	}

	res := DiffHTTP(f, HTTPActual{
		Status: 200,
		Header: map[string]string{"Content-Type": "text/plain"},
	}, nil)
	if res.Pass {
		t.Fatal("expected fail for header mismatch")
	}
	if !strings.Contains(res.Reason, "header mismatch") {
		t.Fatalf("expected header mismatch reason, got: %s", res.Reason)
	}
}

func TestDiffHTTP_HeaderIgnored(t *testing.T) {
	f := Fixture{
		Kind: FixtureHTTP,
		Name: "header_ignore_test",
		HTTP: &HTTPFixture{
			ExpectedStatus: 200,
			ExpectedHeader: map[string]string{"X-Request-Id": "abc123"},
		},
	}

	rules := &NormalizeRules{IgnoreHeaderFields: []string{"X-Request-Id"}}
	res := DiffHTTP(f, HTTPActual{
		Status: 200,
		Header: map[string]string{"X-Request-Id": "different"},
	}, rules)
	if !res.Pass {
		t.Fatalf("expected pass with ignored header, got: %+v", res)
	}
}

func TestDiffHTTP_BodyMismatch(t *testing.T) {
	f := Fixture{
		Kind: FixtureHTTP,
		Name: "body_test",
		HTTP: &HTTPFixture{
			ExpectedStatus: 200,
			ExpectedBody:   []byte(`{"result":"success"}`),
		},
	}

	res := DiffHTTP(f, HTTPActual{
		Status: 200,
		Body:   []byte(`{"result":"failure"}`),
	}, nil)
	if res.Pass {
		t.Fatal("expected fail for body mismatch")
	}
	if !strings.Contains(res.Reason, "body mismatch") {
		t.Fatalf("expected body mismatch reason, got: %s", res.Reason)
	}
}

func TestDiffHTTP_BodyMatch_DifferentOrder(t *testing.T) {
	f := Fixture{
		Kind: FixtureHTTP,
		Name: "body_order_test",
		HTTP: &HTTPFixture{
			ExpectedStatus: 200,
			ExpectedBody:   []byte(`{"a":1,"b":2}`),
		},
	}

	res := DiffHTTP(f, HTTPActual{
		Status: 200,
		Body:   []byte(`{"b":2,"a":1}`),
	}, nil)
	if !res.Pass {
		t.Fatalf("expected pass for same JSON different order, got: %+v", res)
	}
}

func TestDiffHTTP_MissingHTTPFixture(t *testing.T) {
	f := Fixture{Kind: FixtureHTTP, Name: "no_http"}
	res := DiffHTTP(f, HTTPActual{Status: 200}, nil)
	if res.Pass {
		t.Fatal("expected fail for missing HTTP fixture")
	}
}

// ==================== ProofPack Tests ====================

func TestProofPack_AddResult(t *testing.T) {
	pp := NewProofPack()

	f1 := Fixture{Kind: FixtureHTTP, Name: "test1"}
	f2 := Fixture{Kind: FixtureHTTP, Name: "test2"}

	pp.AddResult(f1, DiffResult{Pass: true})
	pp.AddResult(f2, DiffResult{Pass: false, Reason: "failed"})
	pp.Finish()

	if pp.FixtureCount != 2 {
		t.Fatalf("expected 2 fixtures, got %d", pp.FixtureCount)
	}
	if pp.PassCount != 1 {
		t.Fatalf("expected 1 pass, got %d", pp.PassCount)
	}
	if pp.FailCount != 1 {
		t.Fatalf("expected 1 fail, got %d", pp.FailCount)
	}
	if pp.Pass {
		t.Fatal("expected overall fail")
	}
}

func TestProofPack_AllPass(t *testing.T) {
	pp := NewProofPack()

	pp.AddResult(Fixture{Kind: FixtureHTTP, Name: "test1"}, DiffResult{Pass: true})
	pp.AddResult(Fixture{Kind: FixtureHTTP, Name: "test2"}, DiffResult{Pass: true})
	pp.Finish()

	if !pp.Pass {
		t.Fatal("expected overall pass")
	}
}

func TestProofPack_String(t *testing.T) {
	pp := NewProofPack()
	pp.AddResult(Fixture{Kind: FixtureHTTP, Name: "test1"}, DiffResult{Pass: true})

	s := pp.String()
	if !strings.Contains(s, "1 fixtures") {
		t.Fatalf("expected fixture count in string, got: %s", s)
	}
	if !strings.Contains(s, "1 pass") {
		t.Fatalf("expected pass count in string, got: %s", s)
	}
}

// ==================== Normalize Tests ====================

func TestNormalizeRules_IgnoreJSONSet(t *testing.T) {
	rules := &NormalizeRules{
		IgnoreJSONFields: []string{"timestamp", "  request_id  ", ""},
	}

	set := rules.normalizedIgnoreJSONSet()
	if !set["timestamp"] {
		t.Fatal("expected timestamp in set")
	}
	if !set["request_id"] {
		t.Fatal("expected request_id in set (trimmed)")
	}
	if len(set) != 2 {
		t.Fatalf("expected 2 fields, got %d", len(set))
	}
}

func TestNormalizeRules_IgnoreHeaderSet(t *testing.T) {
	rules := &NormalizeRules{
		IgnoreHeaderFields: []string{"X-Request-Id", "  Content-Type  "},
	}

	set := rules.normalizedIgnoreHeaderSet()
	if !set["x-request-id"] {
		t.Fatal("expected x-request-id in set (lowercased)")
	}
	if !set["content-type"] {
		t.Fatal("expected content-type in set (trimmed, lowercased)")
	}
}

// ==================== PII Detector Tests ====================

func TestPIIDetector_DetectSSN(t *testing.T) {
	d := NewPIIDetector(nil)
	matches := d.DetectInText("My SSN is 123-45-6789")

	if len(matches) != 1 {
		t.Fatalf("expected 1 match, got %d", len(matches))
	}
	if matches[0].Type != PIITypeSSN {
		t.Fatalf("expected SSN type, got %s", matches[0].Type)
	}
	if matches[0].Value != "123-45-6789" {
		t.Fatalf("expected SSN value, got %s", matches[0].Value)
	}
}

func TestPIIDetector_DetectEmail(t *testing.T) {
	d := NewPIIDetector(nil)
	matches := d.DetectInText("Contact me at john.doe@example.com")

	if len(matches) != 1 {
		t.Fatalf("expected 1 match, got %d", len(matches))
	}
	if matches[0].Type != PIITypeEmail {
		t.Fatalf("expected email type, got %s", matches[0].Type)
	}
}

func TestPIIDetector_DetectCreditCard(t *testing.T) {
	d := NewPIIDetector(nil)
	matches := d.DetectInText("Card: 4111-1111-1111-1111")

	if len(matches) != 1 {
		t.Fatalf("expected 1 match, got %d", len(matches))
	}
	if matches[0].Type != PIITypeCreditCard {
		t.Fatalf("expected credit card type, got %s", matches[0].Type)
	}
}

func TestPIIDetector_DetectPhone(t *testing.T) {
	d := NewPIIDetector(nil)
	matches := d.DetectInText("Call me at (555) 123-4567")

	if len(matches) != 1 {
		t.Fatalf("expected 1 match, got %d", len(matches))
	}
	if matches[0].Type != PIITypePhone {
		t.Fatalf("expected phone type, got %s", matches[0].Type)
	}
}

func TestPIIDetector_MaskText_Redact(t *testing.T) {
	d := NewPIIDetector(&PIIDetectorConfig{MaskingStyle: MaskingStyleRedact})
	masked, matches := d.MaskText("SSN: 123-45-6789")

	if len(matches) != 1 {
		t.Fatalf("expected 1 match, got %d", len(matches))
	}
	if strings.Contains(masked, "123-45-6789") {
		t.Fatalf("expected SSN to be masked, got: %s", masked)
	}
	if !strings.Contains(masked, "[REDACTED:ssn]") {
		t.Fatalf("expected redaction marker, got: %s", masked)
	}
}

func TestPIIDetector_MaskText_Partial(t *testing.T) {
	d := NewPIIDetector(&PIIDetectorConfig{MaskingStyle: MaskingStylePartial})
	masked, _ := d.MaskText("SSN: 123-45-6789")

	if !strings.Contains(masked, "6789") {
		t.Fatalf("expected last 4 digits visible, got: %s", masked)
	}
}

func TestPIIDetector_MaskText_Hash(t *testing.T) {
	d := NewPIIDetector(&PIIDetectorConfig{MaskingStyle: MaskingStyleHash})
	masked, _ := d.MaskText("SSN: 123-45-6789")

	if !strings.Contains(masked, "HASH:") {
		t.Fatalf("expected hash marker, got: %s", masked)
	}
}

func TestPIIDetector_EnabledTypes(t *testing.T) {
	d := NewPIIDetector(&PIIDetectorConfig{
		EnabledTypes: []PIIType{PIITypeEmail},
	})

	text := "SSN: 123-45-6789, Email: test@example.com"
	matches := d.DetectInText(text)

	if len(matches) != 1 {
		t.Fatalf("expected 1 match (email only), got %d", len(matches))
	}
	if matches[0].Type != PIITypeEmail {
		t.Fatalf("expected email type, got %s", matches[0].Type)
	}
}

func TestPIIDetector_DetectInMap(t *testing.T) {
	d := NewPIIDetector(nil)
	data := map[string]any{
		"name":  "John Doe",
		"email": "john@example.com",
		"nested": map[string]any{
			"ssn": "123-45-6789",
		},
	}

	matches := d.DetectInMap(data)
	if len(matches) < 2 {
		t.Fatalf("expected at least 2 matches, got %d", len(matches))
	}
}

func TestPIIDetector_MaskMap(t *testing.T) {
	d := NewPIIDetector(&PIIDetectorConfig{MaskingStyle: MaskingStyleRedact})
	data := map[string]any{
		"email": "john@example.com",
	}

	masked, matches := d.MaskMap(data)
	if len(matches) != 1 {
		t.Fatalf("expected 1 match, got %d", len(matches))
	}
	if strings.Contains(masked["email"].(string), "john@example.com") {
		t.Fatal("expected email to be masked")
	}
}

// ==================== DBDiff Tests ====================

func TestDBSnapshot_Basic(t *testing.T) {
	snap := &DBSnapshot{
		Timestamp: time.Now(),
		Tables:    make(map[string]*TableSnapshot),
	}

	snap.Tables["users"] = &TableSnapshot{
		Name:     "users",
		RowCount: 2,
		Columns:  []string{"id", "name"},
		Rows: map[string]map[string]any{
			"1": {"id": 1, "name": "Alice"},
			"2": {"id": 2, "name": "Bob"},
		},
	}

	if snap.Tables["users"].RowCount != 2 {
		t.Fatalf("expected 2 rows, got %d", snap.Tables["users"].RowCount)
	}
}

// ==================== Runner Config Tests ====================

func TestDefaultRunnerConfig(t *testing.T) {
	cfg := DefaultRunnerConfig()

	if cfg.Timeout != 5*time.Minute {
		t.Fatalf("expected 5 minute timeout, got %v", cfg.Timeout)
	}
	if cfg.WorkDir != "/tmp/anvil-harness" {
		t.Fatalf("expected default workdir, got %s", cfg.WorkDir)
	}
	if cfg.Env == nil {
		t.Fatal("expected non-nil env map")
	}
}

func TestCOBOLRunner_Name(t *testing.T) {
	r := NewCOBOLRunner(nil)
	if r.Name() != "cobol" {
		t.Fatalf("expected 'cobol', got %s", r.Name())
	}
}

func TestTypeScriptRunner_Name(t *testing.T) {
	r := NewTypeScriptRunner(nil)
	if r.Name() != "typescript" {
		t.Fatalf("expected 'typescript', got %s", r.Name())
	}
}

func TestPythonRunner_Name(t *testing.T) {
	r := NewPythonRunner(nil)
	if r.Name() != "python" {
		t.Fatalf("expected 'python', got %s", r.Name())
	}
}

func TestGoRunner_Name(t *testing.T) {
	r := NewGoRunner(nil)
	if r.Name() != "golang" {
		t.Fatalf("expected 'golang', got %s", r.Name())
	}
}
