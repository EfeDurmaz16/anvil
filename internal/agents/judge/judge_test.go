package judge

import (
	"context"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/llm"
)

type mockProvider struct {
	name    string
	content string
	err     error
}

func (m *mockProvider) Complete(_ context.Context, _ *llm.Prompt, _ *llm.RequestOptions) (*llm.Response, error) {
	if m.err != nil {
		return nil, m.err
	}
	return &llm.Response{Content: m.content}, nil
}

func (m *mockProvider) Embed(_ context.Context, _ []string) ([][]float32, error) { return nil, nil }
func (m *mockProvider) Name() string                                             { return m.name }

func TestJudge_ParsesJSONVerdict(t *testing.T) {
	j := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name: "M",
			Functions: []*ir.Function{{
				Name: "F",
				Body: "MOVE A TO B.",
			}},
		}},
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   &mockProvider{name: "mock", content: `{"equivalent": false, "reason": "missing logic"}`},
		Params: map[string]string{
			"generated_code": "public class X {}",
		},
	}

	res, err := j.Run(context.Background(), ac)
	if err != nil {
		t.Fatal(err)
	}
	if res.Score >= 1.0 {
		t.Fatalf("expected score to decrease, got %f", res.Score)
	}
	if len(res.Errors) == 0 {
		t.Fatal("expected errors")
	}
}

func TestJudge_InvalidJSONCountsAsFailure(t *testing.T) {
	j := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name: "M",
			Functions: []*ir.Function{{
				Name: "F",
				Body: "MOVE A TO B.",
			}},
		}},
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   &mockProvider{name: "mock", content: "sure, looks fine"},
		Params: map[string]string{
			"generated_code": "public class X {}",
		},
	}

	res, err := j.Run(context.Background(), ac)
	if err != nil {
		t.Fatal(err)
	}
	if res.Score >= 1.0 {
		t.Fatalf("expected score to decrease, got %f", res.Score)
	}
	if len(res.Errors) == 0 {
		t.Fatal("expected errors")
	}
}
