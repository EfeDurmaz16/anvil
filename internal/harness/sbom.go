package harness

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"time"
)

// SBOMFormat specifies the SBOM output format.
type SBOMFormat string

const (
	SBOMFormatCycloneDX SBOMFormat = "cyclonedx"
	SBOMFormatSPDX      SBOMFormat = "spdx"
)

// SBOMConfig configures SBOM generation.
type SBOMConfig struct {
	Format      SBOMFormat
	IncludeHash bool
}

// DefaultSBOMConfig returns default SBOM configuration.
func DefaultSBOMConfig() *SBOMConfig {
	return &SBOMConfig{
		Format:      SBOMFormatCycloneDX,
		IncludeHash: true,
	}
}

// CycloneDXBOM represents a CycloneDX 1.5 SBOM.
type CycloneDXBOM struct {
	BOMFormat    string                `json:"bomFormat"`
	SpecVersion  string                `json:"specVersion"`
	SerialNumber string                `json:"serialNumber"`
	Version      int                   `json:"version"`
	Metadata     *CycloneDXMetadata    `json:"metadata"`
	Components   []CycloneDXComponent  `json:"components"`
	Dependencies []CycloneDXDependency `json:"dependencies,omitempty"`
}

// CycloneDXMetadata contains SBOM metadata.
type CycloneDXMetadata struct {
	Timestamp string              `json:"timestamp"`
	Tools     []CycloneDXTool     `json:"tools,omitempty"`
	Component *CycloneDXComponent `json:"component,omitempty"`
}

// CycloneDXTool represents the tool that generated the SBOM.
type CycloneDXTool struct {
	Vendor  string `json:"vendor"`
	Name    string `json:"name"`
	Version string `json:"version"`
}

// CycloneDXComponent represents a software component.
type CycloneDXComponent struct {
	Type       string              `json:"type"`
	BOMRef     string              `json:"bom-ref,omitempty"`
	Name       string              `json:"name"`
	Version    string              `json:"version"`
	Group      string              `json:"group,omitempty"`
	Purl       string              `json:"purl,omitempty"`
	Hashes     []CycloneDXHash     `json:"hashes,omitempty"`
	Licenses   []CycloneDXLicense  `json:"licenses,omitempty"`
	Properties []CycloneDXProperty `json:"properties,omitempty"`
}

// CycloneDXHash represents a file hash.
type CycloneDXHash struct {
	Alg     string `json:"alg"`
	Content string `json:"content"`
}

// CycloneDXLicense represents license information.
type CycloneDXLicense struct {
	License CycloneDXLicenseInfo `json:"license"`
}

// CycloneDXLicenseInfo contains license details.
type CycloneDXLicenseInfo struct {
	ID   string `json:"id,omitempty"`
	Name string `json:"name,omitempty"`
}

// CycloneDXProperty represents a custom property.
type CycloneDXProperty struct {
	Name  string `json:"name"`
	Value string `json:"value"`
}

// CycloneDXDependency represents component dependencies.
type CycloneDXDependency struct {
	Ref       string   `json:"ref"`
	DependsOn []string `json:"dependsOn,omitempty"`
}

// SBOMGenerator generates Software Bill of Materials.
type SBOMGenerator struct {
	config     *SBOMConfig
	components []CycloneDXComponent
}

// NewSBOMGenerator creates a new SBOM generator.
func NewSBOMGenerator(config *SBOMConfig) *SBOMGenerator {
	if config == nil {
		config = DefaultSBOMConfig()
	}
	return &SBOMGenerator{
		config:     config,
		components: []CycloneDXComponent{},
	}
}

// AddGeneratedFile adds a generated file as a component.
func (g *SBOMGenerator) AddGeneratedFile(path string, content []byte, language string) {
	bomRef := fmt.Sprintf("generated-%s", filepath.Base(path))

	component := CycloneDXComponent{
		Type:    "file",
		BOMRef:  bomRef,
		Name:    path,
		Version: "1.0.0",
		Properties: []CycloneDXProperty{
			{Name: "anvil:language", Value: language},
			{Name: "anvil:type", Value: "generated"},
		},
	}

	if g.config.IncludeHash && len(content) > 0 {
		hash := sha256.Sum256(content)
		component.Hashes = []CycloneDXHash{
			{Alg: "SHA-256", Content: hex.EncodeToString(hash[:])},
		}
	}

	g.components = append(g.components, component)
}

// AddSourceFile adds a source file as a component.
func (g *SBOMGenerator) AddSourceFile(path string, content []byte, language string) {
	bomRef := fmt.Sprintf("source-%s", filepath.Base(path))

	component := CycloneDXComponent{
		Type:    "file",
		BOMRef:  bomRef,
		Name:    path,
		Version: "original",
		Properties: []CycloneDXProperty{
			{Name: "anvil:language", Value: language},
			{Name: "anvil:type", Value: "source"},
		},
	}

	if g.config.IncludeHash && len(content) > 0 {
		hash := sha256.Sum256(content)
		component.Hashes = []CycloneDXHash{
			{Alg: "SHA-256", Content: hex.EncodeToString(hash[:])},
		}
	}

	g.components = append(g.components, component)
}

// AddDependency adds an external dependency.
func (g *SBOMGenerator) AddDependency(name, version, purl, license string) {
	bomRef := fmt.Sprintf("dep-%s-%s", name, version)

	component := CycloneDXComponent{
		Type:    "library",
		BOMRef:  bomRef,
		Name:    name,
		Version: version,
		Purl:    purl,
	}

	if license != "" {
		component.Licenses = []CycloneDXLicense{
			{License: CycloneDXLicenseInfo{ID: license}},
		}
	}

	g.components = append(g.components, component)
}

// AddRuntimeDependency adds a runtime dependency for the target language.
func (g *SBOMGenerator) AddRuntimeDependency(language string) {
	switch language {
	case "typescript":
		g.AddDependency("typescript", "5.0.0", "pkg:npm/typescript@5.0.0", "Apache-2.0")
		g.AddDependency("node", "20.0.0", "pkg:generic/node@20.0.0", "MIT")
	case "python":
		g.AddDependency("python", "3.11.0", "pkg:generic/python@3.11.0", "PSF-2.0")
	case "go", "golang":
		g.AddDependency("go", "1.22.0", "pkg:generic/go@1.22.0", "BSD-3-Clause")
	case "java":
		g.AddDependency("openjdk", "21.0.0", "pkg:generic/openjdk@21.0.0", "GPL-2.0-with-classpath-exception")
	}
}

// Generate creates the SBOM and returns it as JSON.
func (g *SBOMGenerator) Generate(projectName, projectVersion string) (*CycloneDXBOM, error) {
	serialNumber := fmt.Sprintf("urn:uuid:%s", generateUUID())

	bom := &CycloneDXBOM{
		BOMFormat:    "CycloneDX",
		SpecVersion:  "1.5",
		SerialNumber: serialNumber,
		Version:      1,
		Metadata: &CycloneDXMetadata{
			Timestamp: time.Now().UTC().Format(time.RFC3339),
			Tools: []CycloneDXTool{
				{
					Vendor:  "Anvil",
					Name:    "anvil",
					Version: "0.1.0",
				},
			},
			Component: &CycloneDXComponent{
				Type:    "application",
				BOMRef:  "anvil-generated",
				Name:    projectName,
				Version: projectVersion,
				Properties: []CycloneDXProperty{
					{Name: "anvil:go-version", Value: runtime.Version()},
					{Name: "anvil:generated-at", Value: time.Now().UTC().Format(time.RFC3339)},
				},
			},
		},
		Components: g.components,
	}

	return bom, nil
}

// WriteToFile writes the SBOM to a file.
func (g *SBOMGenerator) WriteToFile(outputPath, projectName, projectVersion string) error {
	bom, err := g.Generate(projectName, projectVersion)
	if err != nil {
		return err
	}

	data, err := json.MarshalIndent(bom, "", "  ")
	if err != nil {
		return fmt.Errorf("marshal SBOM: %w", err)
	}

	if err := os.MkdirAll(filepath.Dir(outputPath), 0755); err != nil {
		return fmt.Errorf("create SBOM dir: %w", err)
	}

	if err := os.WriteFile(outputPath, data, 0644); err != nil {
		return fmt.Errorf("write SBOM: %w", err)
	}

	return nil
}

// generateUUID generates a simple UUID v4.
func generateUUID() string {
	// Simple timestamp-based UUID for SBOM serial numbers
	now := time.Now().UnixNano()
	hash := sha256.Sum256([]byte(fmt.Sprintf("%d-%d", now, runtime.NumGoroutine())))
	return fmt.Sprintf("%x-%x-%x-%x-%x",
		hash[0:4], hash[4:6], hash[6:8], hash[8:10], hash[10:16])
}
