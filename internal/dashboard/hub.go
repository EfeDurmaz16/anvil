package dashboard

import (
	"encoding/json"
	"fmt"
	"net/http"
	"sync"
	"time"
)

// Hub manages Server-Sent Events (SSE) connections for real-time dashboard updates.
type Hub struct {
	mu      sync.RWMutex
	clients map[*Client]bool
}

// Client represents a single SSE connection.
type Client struct {
	hub     *Hub
	writer  http.ResponseWriter
	flusher http.Flusher
	done    chan struct{}
}

// NewHub creates a new Hub instance.
func NewHub() *Hub {
	return &Hub{
		clients: make(map[*Client]bool),
	}
}

// Run starts the hub (no-op for SSE, kept for API compatibility).
func (h *Hub) Run() {
	// SSE doesn't need a background goroutine like WebSocket does
	// Kept for API compatibility with WebSocket-based implementations
}

// Register adds a new client to the hub.
func (h *Hub) Register(client *Client) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.clients[client] = true
}

// Unregister removes a client from the hub.
func (h *Hub) Unregister(client *Client) {
	h.mu.Lock()
	defer h.mu.Unlock()
	if _, ok := h.clients[client]; ok {
		delete(h.clients, client)
		close(client.done)
	}
}

// Broadcast sends an event to all connected clients.
func (h *Hub) Broadcast(event *Event) {
	h.mu.RLock()
	defer h.mu.RUnlock()

	data, err := json.Marshal(event)
	if err != nil {
		return
	}

	for client := range h.clients {
		select {
		case <-client.done:
			// Client disconnected, skip
		default:
			client.send(data)
		}
	}
}

// NewClient creates a new SSE client from an HTTP response writer.
func NewClient(hub *Hub, w http.ResponseWriter) (*Client, error) {
	flusher, ok := w.(http.Flusher)
	if !ok {
		return nil, fmt.Errorf("streaming not supported")
	}

	// Set SSE headers
	w.Header().Set("Content-Type", "text/event-stream")
	w.Header().Set("Cache-Control", "no-cache")
	w.Header().Set("Connection", "keep-alive")
	w.Header().Set("Access-Control-Allow-Origin", "*")

	client := &Client{
		hub:     hub,
		writer:  w,
		flusher: flusher,
		done:    make(chan struct{}),
	}

	return client, nil
}

// send writes an SSE event to the client.
func (c *Client) send(data []byte) {
	fmt.Fprintf(c.writer, "data: %s\n\n", data)
	c.flusher.Flush()
}

// SendPing sends a ping event to keep the connection alive.
func (c *Client) SendPing() {
	select {
	case <-c.done:
		return
	default:
		fmt.Fprintf(c.writer, ": ping\n\n")
		c.flusher.Flush()
	}
}

// KeepAlive sends periodic pings to keep the SSE connection alive.
func (c *Client) KeepAlive(interval time.Duration) {
	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	for {
		select {
		case <-c.done:
			return
		case <-ticker.C:
			c.SendPing()
		}
	}
}
