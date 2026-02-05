package treesitter

// LanguageFunc is a function that returns a language pointer.
// In CGO mode, this wraps the TSLanguage pointer.
// Each language grammar provides one of these via Register().
type LanguageFunc func() uintptr

// registry holds registered language grammars.
var registry = make(map[string]LanguageFunc)

// Register adds a language grammar to the global registry.
// Call this from init() in language-specific packages.
// Example:
//
//	func init() {
//	    treesitter.Register("cobol", func() uintptr {
//	        return uintptr(unsafe.Pointer(C.tree_sitter_cobol()))
//	    })
//	}
func Register(name string, fn LanguageFunc) {
	registry[name] = fn
}

// GetLanguage looks up a registered language by name.
func GetLanguage(name string) (LanguageFunc, bool) {
	fn, ok := registry[name]
	return fn, ok
}

// Languages returns all registered language names.
func Languages() []string {
	names := make([]string, 0, len(registry))
	for name := range registry {
		names = append(names, name)
	}
	return names
}
