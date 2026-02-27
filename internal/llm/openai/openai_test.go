package openai

import (
	"context"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/llm"
)

func TestNew_SetsDefaults(t *testing.T) {
	client := New("test-key", "gpt-4", "", "")

	if client.apiKey != "test-key" {
		t.Errorf("expected apiKey 'test-key', got %q", client.apiKey)
	}
	if client.model != "gpt-4" {
		t.Errorf("expected model 'gpt-4', got %q", client.model)
	}
	if client.baseURL != defaultBaseURL {
		t.Errorf("expected default baseURL %q, got %q", defaultBaseURL, client.baseURL)
	}
	if client.embedModel != "text-embedding-3-small" {
		t.Errorf("expected default embedModel 'text-embedding-3-small', got %q", client.embedModel)
	}
	if client.http == nil {
		t.Error("expected http client to be initialized")
	}
}

func TestNew_CustomBaseURL(t *testing.T) {
	customURL := "https://custom.api.com/v1"
	client := New("key", "model", customURL, "")

	if client.baseURL != customURL {
		t.Errorf("expected baseURL %q, got %q", customURL, client.baseURL)
	}
}

func TestNew_CustomEmbedModel(t *testing.T) {
	client := New("key", "model", "", "custom-embed-model")

	if client.embedModel != "custom-embed-model" {
		t.Errorf("expected embedModel 'custom-embed-model', got %q", client.embedModel)
	}
}

func TestName(t *testing.T) {
	client := New("key", "model", "", "")
	if client.Name() != "openai" {
		t.Errorf("expected name 'openai', got %q", client.Name())
	}
}

func TestComplete_Success(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodPost {
			t.Errorf("expected POST, got %s", r.Method)
		}
		if r.Header.Get("Authorization") == "" {
			t.Error("missing Authorization header")
		}

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]any{
			"choices": []map[string]any{
				{
					"message":       map[string]string{"content": "hello world"},
					"finish_reason": "stop",
				},
			},
			"model": "gpt-4",
			"usage": map[string]int{
				"prompt_tokens":     10,
				"completion_tokens": 5,
			},
		})
	}))
	defer server.Close()

	client := New("test-key", "gpt-4", server.URL, "")
	prompt := &llm.Prompt{Messages: []llm.Message{{Role: "user", Content: "hi"}}}

	resp, err := client.Complete(context.Background(), prompt, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if resp.Content != "hello world" {
		t.Errorf("expected 'hello world', got %q", resp.Content)
	}
	if resp.Model != "gpt-4" {
		t.Errorf("expected model 'gpt-4', got %q", resp.Model)
	}
	if resp.StopReason != "stop" {
		t.Errorf("expected stop_reason 'stop', got %q", resp.StopReason)
	}
	if resp.InputTokens != 10 {
		t.Errorf("expected 10 input tokens, got %d", resp.InputTokens)
	}
	if resp.OutputTokens != 5 {
		t.Errorf("expected 5 output tokens, got %d", resp.OutputTokens)
	}
}

func TestComplete_SendsAuthorizationHeader(t *testing.T) {
	var capturedAuth string

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		capturedAuth = r.Header.Get("Authorization")
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]any{
			"choices": []map[string]any{
				{"message": map[string]string{"content": "ok"}, "finish_reason": "stop"},
			},
			"model": "gpt-4",
			"usage": map[string]int{"prompt_tokens": 1, "completion_tokens": 1},
		})
	}))
	defer server.Close()

	client := New("my-secret-key", "gpt-4", server.URL, "")
	client.Complete(context.Background(), &llm.Prompt{
		Messages: []llm.Message{{Role: "user", Content: "test"}},
	}, nil)

	if capturedAuth != "Bearer my-secret-key" {
		t.Errorf("expected 'Bearer my-secret-key', got %q", capturedAuth)
	}
}

func TestComplete_SendsCorrectJSONBody(t *testing.T) {
	var capturedBody map[string]any

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		bodyBytes, _ := io.ReadAll(r.Body)
		json.Unmarshal(bodyBytes, &capturedBody)

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]any{
			"choices": []map[string]any{
				{"message": map[string]string{"content": "ok"}, "finish_reason": "stop"},
			},
			"model": "gpt-4",
			"usage": map[string]int{"prompt_tokens": 1, "completion_tokens": 1},
		})
	}))
	defer server.Close()

	client := New("key", "gpt-4", server.URL, "")
	temp := 0.7
	topP := 0.9
	maxTokens := 2048

	client.Complete(context.Background(), &llm.Prompt{
		SystemPrompt: "You are helpful",
		Messages:     []llm.Message{{Role: "user", Content: "Hello"}},
	}, &llm.RequestOptions{
		Temperature: &temp,
		TopP:        &topP,
		MaxTokens:   &maxTokens,
		StopSeqs:    []string{"STOP"},
	})

	if capturedBody["model"] != "gpt-4" {
		t.Errorf("expected model 'gpt-4', got %v", capturedBody["model"])
	}
	if capturedBody["max_tokens"] != float64(2048) {
		t.Errorf("expected max_tokens 2048, got %v", capturedBody["max_tokens"])
	}
	if capturedBody["temperature"] != 0.7 {
		t.Errorf("expected temperature 0.7, got %v", capturedBody["temperature"])
	}
	if capturedBody["top_p"] != 0.9 {
		t.Errorf("expected top_p 0.9, got %v", capturedBody["top_p"])
	}

	// System prompt should be first message with role=system
	messages := capturedBody["messages"].([]interface{})
	if len(messages) != 2 {
		t.Errorf("expected 2 messages (system + user), got %d", len(messages))
	}
	firstMsg := messages[0].(map[string]interface{})
	if firstMsg["role"] != "system" {
		t.Errorf("expected first message role 'system', got %v", firstMsg["role"])
	}
	if firstMsg["content"] != "You are helpful" {
		t.Errorf("expected system content 'You are helpful', got %v", firstMsg["content"])
	}

	stopSeqs := capturedBody["stop"].([]interface{})
	if len(stopSeqs) != 1 || stopSeqs[0] != "STOP" {
		t.Errorf("expected stop ['STOP'], got %v", stopSeqs)
	}
}

func TestComplete_ServerError(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusInternalServerError)
		w.Write([]byte(`{"error": {"message": "server error"}}`))
	}))
	defer server.Close()

	client := New("test-key", "gpt-4", server.URL, "")
	prompt := &llm.Prompt{Messages: []llm.Message{{Role: "user", Content: "hi"}}}

	_, err := client.Complete(context.Background(), prompt, nil)
	if err == nil {
		t.Error("expected error for 500 response")
	}
	if !strings.Contains(err.Error(), "500") {
		t.Errorf("expected error to contain '500', got: %v", err)
	}
}

func TestComplete_Unauthorized(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusUnauthorized)
		w.Write([]byte(`{"error": {"message": "invalid api key"}}`))
	}))
	defer server.Close()

	client := New("bad-key", "gpt-4", server.URL, "")
	_, err := client.Complete(context.Background(), &llm.Prompt{
		Messages: []llm.Message{{Role: "user", Content: "test"}},
	}, nil)

	if err == nil {
		t.Fatal("expected error for 401 response")
	}
	if !strings.Contains(err.Error(), "401") {
		t.Errorf("expected error to contain '401', got: %v", err)
	}
}

func TestComplete_MalformedJSON(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.Write([]byte(`{invalid json`))
	}))
	defer server.Close()

	client := New("key", "gpt-4", server.URL, "")
	_, err := client.Complete(context.Background(), &llm.Prompt{
		Messages: []llm.Message{{Role: "user", Content: "test"}},
	}, nil)

	if err == nil {
		t.Fatal("expected error for malformed JSON")
	}
}

func TestComplete_EmptyChoices(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]any{
			"choices": []map[string]any{},
			"model":   "gpt-4",
			"usage":   map[string]int{"prompt_tokens": 1, "completion_tokens": 0},
		})
	}))
	defer server.Close()

	client := New("key", "gpt-4", server.URL, "")
	resp, err := client.Complete(context.Background(), &llm.Prompt{
		Messages: []llm.Message{{Role: "user", Content: "test"}},
	}, nil)

	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if resp.Content != "" {
		t.Errorf("expected empty content for empty choices, got %q", resp.Content)
	}
}

func TestComplete_DefaultMaxTokens(t *testing.T) {
	var capturedBody map[string]any

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		bodyBytes, _ := io.ReadAll(r.Body)
		json.Unmarshal(bodyBytes, &capturedBody)

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]any{
			"choices": []map[string]any{
				{"message": map[string]string{"content": "ok"}, "finish_reason": "stop"},
			},
			"model": "gpt-4",
			"usage": map[string]int{"prompt_tokens": 1, "completion_tokens": 1},
		})
	}))
	defer server.Close()

	client := New("key", "gpt-4", server.URL, "")
	client.Complete(context.Background(), &llm.Prompt{
		Messages: []llm.Message{{Role: "user", Content: "test"}},
	}, nil)

	// Default max_tokens should be 4096
	if capturedBody["max_tokens"] != float64(4096) {
		t.Errorf("expected default max_tokens 4096, got %v", capturedBody["max_tokens"])
	}
}

func TestEmbed_Success(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/embeddings" {
			t.Errorf("expected /embeddings path, got %s", r.URL.Path)
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]any{
			"data": []map[string]any{
				{"embedding": []float32{0.1, 0.2, 0.3}},
				{"embedding": []float32{0.4, 0.5, 0.6}},
			},
		})
	}))
	defer server.Close()

	client := New("key", "model", server.URL, "")
	embeddings, err := client.Embed(context.Background(), []string{"hello", "world"})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(embeddings) != 2 {
		t.Errorf("expected 2 embeddings, got %d", len(embeddings))
	}
	if len(embeddings[0]) != 3 {
		t.Errorf("expected embedding of length 3, got %d", len(embeddings[0]))
	}
}

func TestEmbed_ServerError(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusInternalServerError)
		w.Write([]byte(`{"error": "internal error"}`))
	}))
	defer server.Close()

	client := New("key", "model", server.URL, "")
	_, err := client.Embed(context.Background(), []string{"hello"})
	if err == nil {
		t.Fatal("expected error for 500 response")
	}
}
