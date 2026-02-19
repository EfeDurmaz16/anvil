package anthropic

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
	client := New("test-key", "test-model", "")

	if client.apiKey != "test-key" {
		t.Errorf("expected apiKey 'test-key', got %q", client.apiKey)
	}
	if client.model != "test-model" {
		t.Errorf("expected model 'test-model', got %q", client.model)
	}
	if client.baseURL != defaultBaseURL {
		t.Errorf("expected default baseURL %q, got %q", defaultBaseURL, client.baseURL)
	}
	if client.http == nil {
		t.Error("expected http client to be initialized")
	}
}

func TestNew_CustomBaseURL(t *testing.T) {
	customURL := "https://custom.api.com/v1"
	client := New("key", "model", customURL)

	if client.baseURL != customURL {
		t.Errorf("expected baseURL %q, got %q", customURL, client.baseURL)
	}
}

func TestName(t *testing.T) {
	client := New("key", "model", "")
	if client.Name() != "anthropic" {
		t.Errorf("expected name 'anthropic', got %q", client.Name())
	}
}

func TestComplete_CorrectHeaders(t *testing.T) {
	var capturedHeaders http.Header

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		capturedHeaders = r.Header
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]any{
			"content": []map[string]string{{"text": "response"}},
			"model":   "test-model",
			"usage":   map[string]int{"input_tokens": 10, "output_tokens": 20},
		})
	}))
	defer server.Close()

	client := New("test-api-key", "model", server.URL)
	client.Complete(context.Background(), &llm.Prompt{
		Messages: []llm.Message{{Role: "user", Content: "test"}},
	}, nil)

	if capturedHeaders.Get("x-api-key") != "test-api-key" {
		t.Errorf("expected x-api-key 'test-api-key', got %q", capturedHeaders.Get("x-api-key"))
	}
	if capturedHeaders.Get("anthropic-version") != "2023-06-01" {
		t.Errorf("expected anthropic-version '2023-06-01', got %q", capturedHeaders.Get("anthropic-version"))
	}
	if capturedHeaders.Get("Content-Type") != "application/json" {
		t.Errorf("expected Content-Type 'application/json', got %q", capturedHeaders.Get("Content-Type"))
	}
}

func TestComplete_CorrectJSONBody(t *testing.T) {
	var capturedBody map[string]any

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		bodyBytes, _ := io.ReadAll(r.Body)
		json.Unmarshal(bodyBytes, &capturedBody)

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]any{
			"content": []map[string]string{{"text": "response"}},
			"model":   "test-model",
			"usage":   map[string]int{"input_tokens": 10, "output_tokens": 20},
		})
	}))
	defer server.Close()

	client := New("key", "test-model", server.URL)
	temp := 0.7
	topP := 0.9
	maxTokens := 2048

	client.Complete(context.Background(), &llm.Prompt{
		SystemPrompt: "You are a helpful assistant",
		Messages: []llm.Message{
			{Role: "user", Content: "Hello"},
		},
	}, &llm.RequestOptions{
		Temperature: &temp,
		TopP:        &topP,
		MaxTokens:   &maxTokens,
		StopSeqs:    []string{"STOP"},
	})

	if capturedBody["model"] != "test-model" {
		t.Errorf("expected model 'test-model', got %v", capturedBody["model"])
	}
	if capturedBody["max_tokens"] != float64(2048) {
		t.Errorf("expected max_tokens 2048, got %v", capturedBody["max_tokens"])
	}
	if capturedBody["system"] != "You are a helpful assistant" {
		t.Errorf("expected system prompt, got %v", capturedBody["system"])
	}
	if capturedBody["temperature"] != 0.7 {
		t.Errorf("expected temperature 0.7, got %v", capturedBody["temperature"])
	}
	if capturedBody["top_p"] != 0.9 {
		t.Errorf("expected top_p 0.9, got %v", capturedBody["top_p"])
	}

	messages := capturedBody["messages"].([]interface{})
	if len(messages) != 1 {
		t.Errorf("expected 1 message, got %d", len(messages))
	}

	stopSeqs := capturedBody["stop_sequences"].([]interface{})
	if len(stopSeqs) != 1 || stopSeqs[0] != "STOP" {
		t.Errorf("expected stop_sequences ['STOP'], got %v", stopSeqs)
	}
}

func TestComplete_ParsesSuccessfulResponse(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]any{
			"content": []map[string]string{
				{"text": "This is the response"},
			},
			"model":       "claude-3-opus",
			"stop_reason": "end_turn",
			"usage": map[string]int{
				"input_tokens":  100,
				"output_tokens": 50,
			},
		})
	}))
	defer server.Close()

	client := New("key", "model", server.URL)
	resp, err := client.Complete(context.Background(), &llm.Prompt{
		Messages: []llm.Message{{Role: "user", Content: "test"}},
	}, nil)

	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if resp.Content != "This is the response" {
		t.Errorf("expected content 'This is the response', got %q", resp.Content)
	}
	if resp.Model != "claude-3-opus" {
		t.Errorf("expected model 'claude-3-opus', got %q", resp.Model)
	}
	if resp.StopReason != "end_turn" {
		t.Errorf("expected stop_reason 'end_turn', got %q", resp.StopReason)
	}
	if resp.InputTokens != 100 {
		t.Errorf("expected 100 input tokens, got %d", resp.InputTokens)
	}
	if resp.OutputTokens != 50 {
		t.Errorf("expected 50 output tokens, got %d", resp.OutputTokens)
	}
}

func TestComplete_HandlesNon200StatusCode(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusUnauthorized)
		w.Write([]byte(`{"error": "invalid api key"}`))
	}))
	defer server.Close()

	client := New("bad-key", "model", server.URL)
	_, err := client.Complete(context.Background(), &llm.Prompt{
		Messages: []llm.Message{{Role: "user", Content: "test"}},
	}, nil)

	if err == nil {
		t.Fatal("expected error for non-200 status")
	}
	if !strings.Contains(err.Error(), "401") {
		t.Errorf("expected error to contain '401', got: %v", err)
	}
}

func TestComplete_HandlesMalformedJSON(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.Write([]byte(`{invalid json`))
	}))
	defer server.Close()

	client := New("key", "model", server.URL)
	_, err := client.Complete(context.Background(), &llm.Prompt{
		Messages: []llm.Message{{Role: "user", Content: "test"}},
	}, nil)

	if err == nil {
		t.Fatal("expected error for malformed JSON")
	}
}

func TestComplete_PassesRequestOptions(t *testing.T) {
	var capturedBody map[string]any

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		bodyBytes, _ := io.ReadAll(r.Body)
		json.Unmarshal(bodyBytes, &capturedBody)

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]any{
			"content": []map[string]string{{"text": "response"}},
			"model":   "test-model",
			"usage":   map[string]int{"input_tokens": 10, "output_tokens": 20},
		})
	}))
	defer server.Close()

	client := New("key", "model", server.URL)
	temp := 0.5
	maxTokens := 1000
	topP := 0.95

	client.Complete(context.Background(), &llm.Prompt{
		Messages: []llm.Message{{Role: "user", Content: "test"}},
	}, &llm.RequestOptions{
		Temperature: &temp,
		MaxTokens:   &maxTokens,
		TopP:        &topP,
		StopSeqs:    []string{"END", "STOP"},
	})

	if capturedBody["temperature"] != 0.5 {
		t.Errorf("expected temperature 0.5, got %v", capturedBody["temperature"])
	}
	if capturedBody["max_tokens"] != float64(1000) {
		t.Errorf("expected max_tokens 1000, got %v", capturedBody["max_tokens"])
	}
	if capturedBody["top_p"] != 0.95 {
		t.Errorf("expected top_p 0.95, got %v", capturedBody["top_p"])
	}

	stopSeqs := capturedBody["stop_sequences"].([]interface{})
	if len(stopSeqs) != 2 {
		t.Errorf("expected 2 stop sequences, got %d", len(stopSeqs))
	}
}

func TestEmbed_ReturnsError(t *testing.T) {
	client := New("key", "model", "")
	_, err := client.Embed(context.Background(), []string{"text"})

	if err == nil {
		t.Fatal("expected error for Embed call")
	}
	if !strings.Contains(err.Error(), "not supported") {
		t.Errorf("expected 'not supported' in error, got: %v", err)
	}
}
