package openai

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"time"

	"github.com/efebarandurmaz/anvil/internal/llm"
)

const defaultBaseURL = "https://api.openai.com/v1"

// Client implements llm.Provider for OpenAI-compatible APIs (OpenAI, vLLM, etc.).
type Client struct {
	apiKey  string
	model   string
	baseURL string
	embedModel string
	http    *http.Client
}

// New creates an OpenAI-compatible provider.
func New(apiKey, model, baseURL, embedModel string) *Client {
	if baseURL == "" {
		baseURL = defaultBaseURL
	}
	if embedModel == "" {
		embedModel = "text-embedding-3-small"
	}
	return &Client{
		apiKey:     apiKey,
		model:      model,
		baseURL:    baseURL,
		embedModel: embedModel,
		http:       &http.Client{Timeout: 300 * time.Second},
	}
}

func (c *Client) Name() string { return "openai" }

func (c *Client) Complete(ctx context.Context, prompt *llm.Prompt, opts *llm.RequestOptions) (*llm.Response, error) {
	var msgs []map[string]string
	if prompt.SystemPrompt != "" {
		msgs = append(msgs, map[string]string{"role": "system", "content": prompt.SystemPrompt})
	}
	for _, m := range prompt.Messages {
		msgs = append(msgs, map[string]string{"role": string(m.Role), "content": m.Content})
	}

	body := map[string]any{
		"model":      c.model,
		"messages":   msgs,
		"max_tokens": 4096, // sensible default for all providers
	}
	if opts != nil {
		if opts.MaxTokens != nil {
			body["max_tokens"] = *opts.MaxTokens
		}
		if opts.Temperature != nil {
			body["temperature"] = *opts.Temperature
		}
		if opts.TopP != nil {
			body["top_p"] = *opts.TopP
		}
		if len(opts.StopSeqs) > 0 {
			body["stop"] = opts.StopSeqs
		}
	}

	data, err := json.Marshal(body)
	if err != nil {
		return nil, err
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodPost, c.baseURL+"/chat/completions", bytes.NewReader(data))
	if err != nil {
		return nil, err
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "Bearer "+c.apiKey)

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
		return nil, fmt.Errorf("openai: %s: %s", resp.Status, respBody)
	}

	var result struct {
		Choices []struct {
			Message struct {
				Content string `json:"content"`
			} `json:"message"`
			FinishReason string `json:"finish_reason"`
		} `json:"choices"`
		Model string `json:"model"`
		Usage struct {
			PromptTokens     int `json:"prompt_tokens"`
			CompletionTokens int `json:"completion_tokens"`
		} `json:"usage"`
	}
	if err := json.Unmarshal(respBody, &result); err != nil {
		return nil, err
	}

	text := ""
	stop := ""
	if len(result.Choices) > 0 {
		text = result.Choices[0].Message.Content
		stop = result.Choices[0].FinishReason
	}

	return &llm.Response{
		Content:      text,
		Model:        result.Model,
		InputTokens:  result.Usage.PromptTokens,
		OutputTokens: result.Usage.CompletionTokens,
		StopReason:   stop,
	}, nil
}

func (c *Client) Embed(ctx context.Context, texts []string) ([][]float32, error) {
	body := map[string]any{
		"model": c.embedModel,
		"input": texts,
	}
	data, err := json.Marshal(body)
	if err != nil {
		return nil, err
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodPost, c.baseURL+"/embeddings", bytes.NewReader(data))
	if err != nil {
		return nil, err
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "Bearer "+c.apiKey)

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
		return nil, fmt.Errorf("openai embed: %s: %s", resp.Status, respBody)
	}

	var result struct {
		Data []struct {
			Embedding []float32 `json:"embedding"`
		} `json:"data"`
	}
	if err := json.Unmarshal(respBody, &result); err != nil {
		return nil, err
	}

	embeddings := make([][]float32, len(result.Data))
	for i, d := range result.Data {
		embeddings[i] = d.Embedding
	}
	return embeddings, nil
}
