package dashboard

// Dashboard ties together all dashboard components.
type Dashboard struct {
	Server  *Server
	Store   *Store
	Hub     *Hub
	Emitter *Emitter
}

// New creates a fully wired dashboard.
func New(config *Config) *Dashboard {
	store := NewStore()
	hub := NewHub()
	emitter := NewEmitter(store, hub)
	server := NewServer(config, store, hub)

	return &Dashboard{
		Server:  server,
		Store:   store,
		Hub:     hub,
		Emitter: emitter,
	}
}
