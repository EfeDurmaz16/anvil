# Anvil - Enterprise-grade modernization platform
# Multi-stage build for minimal runtime image.

# Stage 1: Build
FROM golang:1.23-alpine AS builder

RUN apk add --no-cache git ca-certificates

WORKDIR /app

# Copy go mod files first for layer caching
COPY go.mod go.sum ./
RUN go mod download

# Copy source code
COPY . .

# Build binaries (CGO disabled by default; tree-sitter requires explicit build tags).
RUN mkdir -p /out && \
    CGO_ENABLED=0 GOOS=linux go build -trimpath -ldflags="-w -s" -o /out/anvil ./cmd/anvil && \
    CGO_ENABLED=0 GOOS=linux go build -trimpath -ldflags="-w -s" -o /out/anvil-worker ./cmd/worker

# Stage 2: Runtime
FROM alpine:3.19

RUN apk add --no-cache ca-certificates tzdata

# Create non-root user
RUN adduser -D -g '' anvil
USER anvil

WORKDIR /app

# Copy binaries and default configs
COPY --from=builder /out/anvil /usr/local/bin/anvil
COPY --from=builder /out/anvil-worker /usr/local/bin/anvil-worker
COPY --from=builder /app/configs /app/configs

ENTRYPOINT ["anvil"]
CMD ["--help"]
