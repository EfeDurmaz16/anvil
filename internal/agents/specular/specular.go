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
type Specular struct{}

func New() *Specular { return &Specular{} }

func (s *Specular) Name() string { return "specular" }

func (s *Specular) Run(ctx context.Context, ac *agents.AgentContext) (*agents.AgentResult, error) {
	if ac.Graph == nil {
		return nil, fmt.Errorf("specular: no graph provided")
	}
	if ac.LLM == nil {
		return nil, fmt.Errorf("specular: no LLM provider")
	}

	for _, mod := range ac.Graph.Modules {
		for _, fn := range mod.Functions {
			rules, err := extractRules(ctx, ac.LLM, mod.Name, fn)
			if err != nil {
				return &agents.AgentResult{
					Graph:  ac.Graph,
					Errors: []string{fmt.Sprintf("rule extraction for %s.%s: %v", mod.Name, fn.Name, err)},
				}, nil
			}
			ac.Graph.BusinessRules = append(ac.Graph.BusinessRules, rules...)
		}
	}

	return &agents.AgentResult{Graph: ac.Graph}, nil
}

func extractRules(ctx context.Context, provider llm.Provider, module string, fn *ir.Function) ([]*ir.BusinessRule, error) {
	prompt := &llm.Prompt{
		SystemPrompt: "You are a COBOL business rule extraction expert. Given a function body, extract business rules as JSON array with fields: id, description, confidence (0-1), tags.",
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
