package llm

// RequestOptions configures a single LLM request.
type RequestOptions struct {
	Temperature *float64 `json:"temperature,omitempty"`
	MaxTokens   *int     `json:"max_tokens,omitempty"`
	TopP        *float64 `json:"top_p,omitempty"`
	StopSeqs    []string `json:"stop_sequences,omitempty"`
}
