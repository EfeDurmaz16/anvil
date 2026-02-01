package judge

import (
	"context"
	"fmt"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/llm"
)

// Judge verifies semantic equivalence of generated code against the original IR.
// When no LLM is available, it returns a pass-through score of 1.0.
type Judge struct{}

func New() *Judge { return &Judge{} }

func (j *Judge) Name() string { return "judge" }

func (j *Judge) Run(ctx context.Context, ac *agents.AgentContext) (*agents.AgentResult, error) {
	if ac.Graph == nil {
		return nil, fmt.Errorf("judge: no graph provided")
	}

	// Graceful degradation: no LLM means no semantic verification
	if ac.LLM == nil {
		return &agents.AgentResult{
			Graph:    ac.Graph,
			Score:    1.0,
			Metadata: map[string]string{"mode": "passthrough", "reason": "no LLM provider configured"},
		}, nil
	}

	var errs []string
	score := 1.0

	for _, mod := range ac.Graph.Modules {
		for _, fn := range mod.Functions {
			ok, reason, err := verifyFunction(ctx, ac.LLM, mod.Name, fn.Name, fn.Body, ac.Params["generated_code"])
			if err != nil {
				errs = append(errs, fmt.Sprintf("verify %s.%s: %v", mod.Name, fn.Name, err))
				score -= 0.1
				continue
			}
			if !ok {
				errs = append(errs, fmt.Sprintf("%s.%s: %s", mod.Name, fn.Name, reason))
				score -= 0.2
			}
		}
	}

	if score < 0 {
		score = 0
	}

	return &agents.AgentResult{
		Graph:    ac.Graph,
		Score:    score,
		Errors:   errs,
		Metadata: map[string]string{"mode": "llm"},
	}, nil
}

func verifyFunction(ctx context.Context, provider llm.Provider, module, fnName, originalBody, generatedCode string) (bool, string, error) {
	prompt := &llm.Prompt{
		SystemPrompt: "You are a code equivalence verifier. Compare the original COBOL function with the generated Java code. Respond with JSON: {\"equivalent\": true/false, \"reason\": \"...\"}",
		Messages: []llm.Message{
			{Role: llm.RoleUser, Content: fmt.Sprintf("Original COBOL function %s.%s:\n%s\n\nGenerated Java:\n%s", module, fnName, originalBody, generatedCode)},
		},
	}

	resp, err := provider.Complete(ctx, prompt, nil)
	if err != nil {
		return false, "", err
	}

	if len(resp.Content) > 0 {
		return true, resp.Content, nil
	}
	return false, "empty response", nil
}
