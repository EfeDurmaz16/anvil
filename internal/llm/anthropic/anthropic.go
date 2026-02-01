package anthropic

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"

	"github.com/efebarandurmaz/anvil/internal/llm"
)

const defaultBaseURL = "https://api.anthropic.com/v1"

// Client implements llm.Provider for the Anthropic Messages API.
type Client struct {
	apiKey  string
	model   string
	baseURL string
	http    *http.Client
}

// New creates an Anthropic provider.
func New(apiKey, model, baseURL string) *Client {
	if baseURL == "" {
		baseURL = defaultBaseURL
	}
	return &Client{
		apiKey:  apiKey,
		model:   model,
		baseURL: baseURL,
		http:    &http.Client{},
	}
}

func (c *Client) Name() string { return "anthropic" }

func (c *Client) Complete(ctx context.Context, prompt *llm.Prompt, opts *llm.RequestOptions) (*llm.Response, error) {
	maxTokens := 4096
	if opts != nil && opts.MaxTokens != nil {
		maxTokens = *opts.MaxTokens
	}

	body := map[string]any{
		"model":      c.model,
		"max_tokens": maxTokens,
	}
	if prompt.SystemPrompt != "" {
		body["system"] = prompt.SystemPrompt
	}

	msgs := make([]map[string]string, len(prompt.Messages))
	for i, m := range prompt.Messages {
		msgs[i] = map[string]string{"role": string(m.Role), "content": m.Content}
	}
	body["messages"] = msgs

	if opts != nil {
		if opts.Temperature != nil {
			body["temperature"] = *opts.Temperature
		}
		if opts.TopP != nil {
			body["top_p"] = *opts.TopP
		}
		if len(opts.StopSeqs) > 0 {
			body["stop_sequences"] = opts.StopSeqs
		}
	}

	data, err := json.Marshal(body)
	if err != nil {
		return nil, err
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodPost, c.baseURL+"/messages", bytes.NewReader(data))
	if err != nil {
		return nil, err
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("x-api-key", c.apiKey)
	req.Header.Set("anthropic-version", "2023-06-01")

	resp, err := c.http.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("anthropic: %s: %s", resp.Status, respBody)
	}

	var result struct {
		Content []struct {
			Text string `json:"text"`
		} `json:"content"`
		Model      string `json:"model"`
		StopReason string `json:"stop_reason"`
		Usage      struct {
			InputTokens  int `json:"input_tokens"`
			OutputTokens int `json:"output_tokens"`
		} `json:"usage"`
	}
	if err := json.Unmarshal(respBody, &result); err != nil {
		return nil, err
	}

	text := ""
	if len(result.Content) > 0 {
		text = result.Content[0].Text
	}

	return &llm.Response{
		Content:      text,
		Model:        result.Model,
		InputTokens:  result.Usage.InputTokens,
		OutputTokens: result.Usage.OutputTokens,
		StopReason:   result.StopReason,
	}, nil
}

func (c *Client) Embed(_ context.Context, _ []string) ([][]float32, error) {
	return nil, fmt.Errorf("anthropic: embedding not supported, use a dedicated embedding provider")
}
