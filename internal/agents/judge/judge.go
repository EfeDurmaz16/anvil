package judge

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// Judge verifies semantic equivalence of generated code against the original IR.
// When no LLM is available, it returns a pass-through score of 1.0.
type Judge struct{}

func New() *Judge { return &Judge{} }

func (j *Judge) Name() string { return "judge" }

func (j *Judge) Run(ctx context.Context, ac *agents.AgentContext) (*agents.AgentResult, error) {
	result := agents.NewAgentResult()
	result.Graph = ac.Graph

	if ac.Graph == nil {
		result.Status = agents.StatusFailed
		result.AddError("judge: no graph provided")
		result.Finalize()
		return result, fmt.Errorf("judge: no graph provided")
	}

	// Count total functions for metrics
	totalFunctions := 0
	for _, mod := range ac.Graph.Modules {
		totalFunctions += len(mod.Functions)
	}
	result.Metrics.InputItems = totalFunctions

	// Graceful degradation: no LLM means no semantic verification
	if ac.LLM == nil {
		result.SetPassthrough("no LLM provider configured")
		result.Score = 1.0
		result.Metrics.OutputItems = totalFunctions
		result.Finalize()
		return result, nil
	}

	result.Metadata["mode"] = "llm"
	sourceLang := strings.TrimSpace(ac.Params["source"])
	if sourceLang == "" {
		sourceLang = "legacy"
	}
	targetLang := strings.TrimSpace(ac.Params["target"])
	if targetLang == "" {
		targetLang = "target"
	}

	generatedInput := ac.Params["generated_files"]
	if generatedInput == "" {
		generatedInput = ac.Params["generated_code"]
	}
	generatedText := formatGeneratedInput(generatedInput, 64*1024) // keep prompt bounded
	if strings.TrimSpace(generatedText) == "" {
		result.AddWarning("judge: generated code input was empty")
	}

	score := 1.0
	verified := 0
	failed := 0

	for _, mod := range ac.Graph.Modules {
		for _, fn := range mod.Functions {
			ok, reason, err := verifyFunction(ctx, ac.LLM, sourceLang, targetLang, mod.Name, fn.Name, fn.Body, generatedText)
			result.Metrics.LLMCalls++

			if err != nil {
				result.AddError(fmt.Sprintf("verify %s.%s: %v", mod.Name, fn.Name, err))
				score -= 0.1
				failed++
				continue
			}
			if !ok {
				result.AddError(fmt.Sprintf("%s.%s: %s", mod.Name, fn.Name, reason))
				score -= 0.2
				failed++
			} else {
				verified++
			}
		}
	}

	if score < 0 {
		score = 0
	}

	result.Score = score
	result.Metrics.OutputItems = verified
	result.Metrics.SkippedItems = failed
	result.Metrics.FunctionalScore = score

	// Set final status based on score
	if score >= 0.8 {
		result.Status = agents.StatusSuccess
	} else if score > 0 {
		result.Status = agents.StatusPartial
	} else {
		result.Status = agents.StatusFailed
	}

	result.Finalize()
	return result, nil
}

type verdict struct {
	Equivalent bool   `json:"equivalent"`
	Reason     string `json:"reason"`
}

func verifyFunction(ctx context.Context, provider llm.Provider, sourceLang, targetLang, module, fnName, originalBody, generatedCode string) (bool, string, error) {
	prompt := &llm.Prompt{
		SystemPrompt: fmt.Sprintf(
			"You are a code equivalence verifier. Compare the original %s function with the generated %s code for semantic equivalence. "+
				"Respond with STRICT JSON only: {\"equivalent\": true/false, \"reason\": \"...\"}. No extra text.",
			sourceLang, targetLang,
		),
		Messages: []llm.Message{
			{Role: llm.RoleUser, Content: fmt.Sprintf("Original %s function %s.%s:\n%s\n\nGenerated %s code (excerpt):\n%s", sourceLang, module, fnName, originalBody, targetLang, generatedCode)},
		},
	}

	resp, err := provider.Complete(ctx, prompt, nil)
	if err != nil {
		return false, "", err
	}

	text := strings.TrimSpace(resp.Content)
	if text == "" {
		return false, "empty response", nil
	}

	v, err := parseVerdict(text)
	if err != nil {
		return false, "", err
	}
	reason := strings.TrimSpace(v.Reason)
	if reason == "" {
		if v.Equivalent {
			reason = "equivalent"
		} else {
			reason = "not equivalent"
		}
	}
	return v.Equivalent, reason, nil
}

func formatGeneratedInput(input string, maxBytes int) string {
	in := strings.TrimSpace(input)
	if in == "" {
		return ""
	}

	// If this looks like a JSON array, try to parse it as generated files.
	if strings.HasPrefix(in, "[") {
		var files []plugins.GeneratedFile
		if err := json.Unmarshal([]byte(in), &files); err == nil {
			var b strings.Builder
			for _, f := range files {
				if f.Path == "" || len(f.Content) == 0 {
					continue
				}
				b.WriteString("=== ")
				b.WriteString(f.Path)
				b.WriteString(" ===\n")
				b.Write(f.Content)
				if f.Content[len(f.Content)-1] != '\n' {
					b.WriteString("\n")
				}
				if maxBytes > 0 && b.Len() >= maxBytes {
					out := b.String()
					if len(out) > maxBytes {
						out = out[:maxBytes] + "\n…(truncated)…\n"
					}
					return out
				}
			}
			out := b.String()
			if maxBytes > 0 && len(out) > maxBytes {
				out = out[:maxBytes] + "\n…(truncated)…\n"
			}
			return out
		}
	}

	if maxBytes > 0 && len(in) > maxBytes {
		return in[:maxBytes] + "\n…(truncated)…\n"
	}
	return in
}

func parseVerdict(text string) (*verdict, error) {
	var v verdict
	if err := json.Unmarshal([]byte(text), &v); err == nil {
		return &v, nil
	}

	// Try to extract a JSON object from surrounding text.
	start := strings.IndexByte(text, '{')
	end := strings.LastIndexByte(text, '}')
	if start >= 0 && end > start {
		if err := json.Unmarshal([]byte(text[start:end+1]), &v); err == nil {
			return &v, nil
		}
	}

	return nil, fmt.Errorf("judge: invalid JSON verdict: %q", truncate(text, 200))
}

func truncate(s string, n int) string {
	if n <= 0 || len(s) <= n {
		return s
	}
	return s[:n] + "…"
}
