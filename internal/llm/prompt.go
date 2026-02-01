package llm

// Role identifies who authored a message.
type Role string

const (
	RoleSystem    Role = "system"
	RoleUser      Role = "user"
	RoleAssistant Role = "assistant"
)

// Message is a single turn in a conversation.
type Message struct {
	Role    Role   `json:"role"`
	Content string `json:"content"`
}

// Prompt is the full input to an LLM completion call.
type Prompt struct {
	SystemPrompt string    `json:"system_prompt,omitempty"`
	Messages     []Message `json:"messages"`
}
