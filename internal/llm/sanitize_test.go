package llm

import "testing"

func TestStripThinkingTags_NoTags(t *testing.T) {
	input := "This is a normal response without any thinking tags."
	result := StripThinkingTags(input)

	if result != input {
		t.Errorf("expected unchanged output, got: %q", result)
	}
}

func TestStripThinkingTags_SingleBlock(t *testing.T) {
	input := "Here is my answer: <think>internal reasoning here</think> The final result."
	expected := "Here is my answer:  The final result."

	result := StripThinkingTags(input)
	if result != expected {
		t.Errorf("expected %q, got %q", expected, result)
	}
}

func TestStripThinkingTags_MultipleBlocks(t *testing.T) {
	input := "First <think>reasoning 1</think> middle <think>reasoning 2</think> end."
	expected := "First  middle  end."

	result := StripThinkingTags(input)
	if result != expected {
		t.Errorf("expected %q, got %q", expected, result)
	}
}

func TestStripThinkingTags_UnclosedTag(t *testing.T) {
	input := "Some text before <think>reasoning that never ends"
	expected := "Some text before"

	result := StripThinkingTags(input)
	if result != expected {
		t.Errorf("expected %q, got %q", expected, result)
	}
}

func TestStripThinkingTags_NestedContent(t *testing.T) {
	input := "<think>Step 1: analyze\nStep 2: decide</think>Final answer"
	expected := "Final answer"

	result := StripThinkingTags(input)
	if result != expected {
		t.Errorf("expected %q, got %q", expected, result)
	}
}

func TestStripThinkingTags_EmptyString(t *testing.T) {
	input := ""
	result := StripThinkingTags(input)

	if result != "" {
		t.Errorf("expected empty string, got %q", result)
	}
}

func TestStripThinkingTags_OnlyTags(t *testing.T) {
	input := "<think>just thinking</think>"
	expected := ""

	result := StripThinkingTags(input)
	if result != expected {
		t.Errorf("expected empty string, got %q", result)
	}
}

func TestStripThinkingTags_WhitespaceHandling(t *testing.T) {
	input := "  \n  <think>thoughts</think>  \n  Answer  \n  "
	expected := "Answer"

	result := StripThinkingTags(input)
	if result != expected {
		t.Errorf("expected %q, got %q", expected, result)
	}
}

func TestStripThinkingTags_ContentBeforeAndAfter(t *testing.T) {
	input := "Introduction text <think>hidden reasoning process</think> conclusion text"
	expected := "Introduction text  conclusion text"

	result := StripThinkingTags(input)
	if result != expected {
		t.Errorf("expected %q, got %q", expected, result)
	}
}

func TestStripThinkingTags_MultipleUnclosedTags(t *testing.T) {
	// If there's an unclosed tag, everything from that point is removed
	input := "Text <think>first reasoning"
	expected := "Text"

	result := StripThinkingTags(input)
	if result != expected {
		t.Errorf("expected %q, got %q", expected, result)
	}
}
