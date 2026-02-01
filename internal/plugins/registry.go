package plugins

import (
	"fmt"
	"sync"
)

// Registry stores available source and target plugins.
type Registry struct {
	mu      sync.RWMutex
	sources map[string]SourcePlugin
	targets map[string]TargetPlugin
}

// NewRegistry creates an empty plugin registry.
func NewRegistry() *Registry {
	return &Registry{
		sources: make(map[string]SourcePlugin),
		targets: make(map[string]TargetPlugin),
	}
}

func (r *Registry) RegisterSource(p SourcePlugin) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.sources[p.Language()] = p
}

func (r *Registry) RegisterTarget(p TargetPlugin) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.targets[p.Language()] = p
}

func (r *Registry) Source(lang string) (SourcePlugin, error) {
	r.mu.RLock()
	defer r.mu.RUnlock()
	p, ok := r.sources[lang]
	if !ok {
		return nil, fmt.Errorf("no source plugin for language %q", lang)
	}
	return p, nil
}

func (r *Registry) Target(lang string) (TargetPlugin, error) {
	r.mu.RLock()
	defer r.mu.RUnlock()
	p, ok := r.targets[lang]
	if !ok {
		return nil, fmt.Errorf("no target plugin for language %q", lang)
	}
	return p, nil
}
