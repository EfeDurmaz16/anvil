package specular

import (
	"context"
	"encoding/json"
	"fmt"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/llm"
)

// Specular uses LLM to extract business rules and enrich the IR.
// When no LLM is available, it passes the graph through unchanged.
type Specular struct{}

func New() *Specular { return &Specular{} }

func (s *Specular) Name() string { return "specular" }

func (s *Specular) Run(ctx context.Context, ac *agents.AgentContext) (*agents.AgentResult, error) {
	result := agents.NewAgentResult()
	result.Graph = ac.Graph

	if ac.Graph == nil {
		result.Status = agents.StatusFailed
		result.AddError("specular: no graph provided")
		result.Finalize()
		return result, fmt.Errorf("specular: no graph provided")
	}

	// Count total functions for metrics
	totalFunctions := 0
	for _, mod := range ac.Graph.Modules {
		totalFunctions += len(mod.Functions)
	}
	result.Metrics.InputItems = totalFunctions

	// Graceful degradation: skip LLM enrichment when no provider is configured
	if ac.LLM == nil {
		result.SetPassthrough("no LLM provider configured")
		result.Metrics.OutputItems = 0
		result.Finalize()
		return result, nil
	}

	result.Metadata["mode"] = "llm"
	rulesExtracted := 0
	failed := 0

	for _, mod := range ac.Graph.Modules {
		for _, fn := range mod.Functions {
			rules, err := extractRules(ctx, ac.LLM, mod.Language, mod.Name, fn)
			result.Metrics.LLMCalls++

			if err != nil {
				result.AddError(fmt.Sprintf("rule extraction for %s.%s: %v", mod.Name, fn.Name, err))
				failed++
				continue
			}
			ac.Graph.BusinessRules = append(ac.Graph.BusinessRules, rules...)
			rulesExtracted += len(rules)
		}
	}

	result.Metrics.OutputItems = rulesExtracted
	result.Metrics.SkippedItems = failed
	result.Metadata["rules_extracted"] = fmt.Sprintf("%d", rulesExtracted)

	// Set status based on success rate
	if failed == 0 {
		result.Status = agents.StatusSuccess
	} else if rulesExtracted > 0 {
		result.Status = agents.StatusPartial
	} else {
		result.Status = agents.StatusFailed
	}

	result.Finalize()
	return result, nil
}

func extractRules(ctx context.Context, provider llm.Provider, sourceLang string, module string, fn *ir.Function) ([]*ir.BusinessRule, error) {
	prompt := &llm.Prompt{
		SystemPrompt: fmt.Sprintf("You are a %s business rule extraction expert. Given a function body, extract business rules as JSON array with fields: id, description, confidence (0-1), tags.", sourceLang),
		Messages: []llm.Message{
			{Role: llm.RoleUser, Content: fmt.Sprintf("Module: %s\nFunction: %s\nBody:\n%s", module, fn.Name, fn.Body)},
		},
	}

	resp, err := provider.Complete(ctx, prompt, nil)
	if err != nil {
		return nil, err
	}

	var rules []*ir.BusinessRule
	if err := json.Unmarshal([]byte(resp.Content), &rules); err != nil {
		return []*ir.BusinessRule{{
			ID:          fmt.Sprintf("%s.%s.rule1", module, fn.Name),
			Description: resp.Content,
			SourceRef:   fmt.Sprintf("%s.%s", module, fn.Name),
			Confidence:  0.5,
		}}, nil
	}

	for _, r := range rules {
		r.SourceRef = fmt.Sprintf("%s.%s", module, fn.Name)
	}
	return rules, nil
}
