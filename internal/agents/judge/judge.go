package judge

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"strconv"
	"strings"
	"sync"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/ir"
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

	verified := 0
	failed := 0

	// Collect all functions to verify
	type verifyJob struct {
		mod *ir.Module
		fn  *ir.Function
	}
	var jobs []verifyJob
	for _, mod := range ac.Graph.Modules {
		for _, fn := range mod.Functions {
			jobs = append(jobs, verifyJob{mod: mod, fn: fn})
		}
	}

	// Verify functions concurrently with worker pool.
	// maxConcurrent defaults to 1 but can be overridden via Params["max_concurrent"].
	maxConcurrent := 1
	if mc, ok := ac.Params["max_concurrent"]; ok {
		if v, err := strconv.Atoi(mc); err == nil && v > 0 {
			maxConcurrent = v
		}
	}
	sem := make(chan struct{}, maxConcurrent)
	var wg sync.WaitGroup
	var mu sync.Mutex

	completed := 0
	for _, job := range jobs {
		wg.Add(1)
		sem <- struct{}{} // acquire
		go func(j verifyJob) {
			defer wg.Done()
			defer func() { <-sem }() // release

			// Extract only the relevant function's generated code to avoid
			// overwhelming smaller LLMs with the entire output.
			fnSnippet := extractFunctionSnippet(generatedText, j.fn.Name)

			ok, reason, err := verifyFunction(ctx, ac.LLM, ac.DefaultOpts, sourceLang, targetLang, j.mod.Name, j.fn.Name, j.fn.Body, fnSnippet)

			mu.Lock()
			completed++
			result.Metrics.LLMCalls++
			if err != nil {
				result.AddError(fmt.Sprintf("verify %s.%s: %v", j.mod.Name, j.fn.Name, err))
				failed++
			} else if !ok {
				result.AddError(fmt.Sprintf("%s.%s: %s", j.mod.Name, j.fn.Name, reason))
				failed++
			} else {
				verified++
			}
			// Print progress for large batches
			if len(jobs) > 10 {
				fmt.Fprintf(os.Stderr, "  [judge] %d/%d verified (%.0f%% pass)\n", completed, len(jobs), float64(verified)/float64(completed)*100)
			}
			mu.Unlock()
		}(job)
	}
	wg.Wait()

	// Percentage-based scoring: ratio of verified functions to total
	score := 0.0
	if len(jobs) > 0 {
		score = float64(verified) / float64(len(jobs))
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

func verifyFunction(ctx context.Context, provider llm.Provider, opts *llm.RequestOptions, sourceLang, targetLang, module, fnName, originalBody, generatedCode string) (bool, string, error) {
	prompt := &llm.Prompt{
		SystemPrompt: fmt.Sprintf(
			"You are a code equivalence verifier. Compare the original %s function with the generated %s code.\n"+
				"You MUST respond with ONLY a JSON object, nothing else.\n\n"+
				"Example response:\n"+
				"{\"equivalent\": true, \"reason\": \"Logic preserved correctly\"}\n\n"+
				"Another example:\n"+
				"{\"equivalent\": false, \"reason\": \"Missing error handling branch\"}\n\n"+
				"Respond with JSON ONLY. No markdown, no explanation, no code fences.",
			sourceLang, targetLang,
		),
		Messages: []llm.Message{
			{Role: llm.RoleUser, Content: fmt.Sprintf("Original %s function %s.%s:\n%s\n\nGenerated %s code (excerpt):\n%s\n\nRespond with JSON only: {\"equivalent\": true/false, \"reason\": \"...\"}", sourceLang, module, fnName, originalBody, targetLang, generatedCode)},
		},
	}

	resp, err := provider.Complete(ctx, prompt, opts)
	if err != nil {
		return false, "", err
	}

	text := strings.TrimSpace(llm.StripThinkingTags(resp.Content))
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

	// Fallback: heuristic analysis of free-text response for models that
	// don't follow JSON instructions (common with smaller LLMs).
	lower := strings.ToLower(text)
	positiveSignals := []string{"equivalent", "semantically correct", "logic is preserved", "correctly translated", "matches the original", "faithful"}
	negativeSignals := []string{"not equivalent", "missing", "incorrect", "differs", "does not match", "lost", "wrong"}

	posScore := 0
	negScore := 0
	for _, sig := range positiveSignals {
		if strings.Contains(lower, sig) {
			posScore++
		}
	}
	for _, sig := range negativeSignals {
		if strings.Contains(lower, sig) {
			negScore++
		}
	}

	if posScore > 0 || negScore > 0 {
		equiv := posScore > negScore
		reason := truncate(text, 150)
		return &verdict{Equivalent: equiv, Reason: reason}, nil
	}

	// If we can't determine anything, assume partial equivalence
	return &verdict{Equivalent: false, Reason: "could not parse verdict: " + truncate(text, 100)}, nil
}

// extractFunctionSnippet finds the generated code for a specific function
// by looking for the method DEFINITION signature (not just any name reference).
// This prevents matching function names in JSDoc comments or method calls.
func extractFunctionSnippet(generatedText, fnName string) string {
	camelName := toCamelCase(fnName)

	// Look for method definition pattern: "  methodName(" at start of line
	defPatterns := []string{
		camelName + "(",            // methodName(
		"async " + camelName + "(", // async methodName(
	}

	lines := strings.Split(generatedText, "\n")
	for _, pattern := range defPatterns {
		for i, line := range lines {
			trimmed := strings.TrimSpace(line)
			// Must be a method definition, not a call or comment
			if !strings.HasPrefix(trimmed, pattern) {
				continue
			}
			// Skip if inside a comment
			if strings.Contains(line, "//") || strings.Contains(line, "*") {
				rawBefore := strings.TrimSpace(strings.Split(line, pattern)[0])
				if strings.HasPrefix(rawBefore, "//") || strings.HasPrefix(rawBefore, "*") {
					continue
				}
			}

			// Found the definition - extract the full method body
			start := i
			// Include JSDoc above if present
			for s := i - 1; s >= 0 && s >= i-6; s-- {
				st := strings.TrimSpace(lines[s])
				if st == "" || strings.HasPrefix(st, "*") || strings.HasPrefix(st, "/**") || strings.HasPrefix(st, "*/") {
					start = s
				} else {
					break
				}
			}

			// Find closing brace by tracking depth
			end := i + 1
			braceDepth := 0
			for j := i; j < len(lines) && j < i+100; j++ {
				braceDepth += strings.Count(lines[j], "{") - strings.Count(lines[j], "}")
				if braceDepth <= 0 && j > i {
					end = j + 1
					break
				}
			}
			return strings.Join(lines[start:end], "\n")
		}
	}

	// Fallback: return truncated full text
	if len(generatedText) > 2000 {
		return generatedText[:2000] + "\n…(truncated)…"
	}
	return generatedText
}

func toCamelCase(name string) string {
	parts := strings.FieldsFunc(name, func(r rune) bool {
		return r == '-' || r == '_' || r == ' ' || r == '.'
	})
	var out string
	for i, p := range parts {
		if p == "" {
			continue
		}
		if i == 0 {
			out += strings.ToLower(p)
		} else {
			out += strings.ToUpper(p[:1]) + strings.ToLower(p[1:])
		}
	}
	return out
}

func truncate(s string, n int) string {
	if n <= 0 || len(s) <= n {
		return s
	}
	return s[:n] + "…"
}
