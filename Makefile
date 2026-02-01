.PHONY: build test lint clean

build:
	go build ./...

test:
	go test ./... -v -count=1

lint:
	golangci-lint run ./...

clean:
	rm -f bin/anvil bin/worker

bin/anvil: cmd/anvil/main.go
	go build -o bin/anvil ./cmd/anvil

bin/worker: cmd/worker/main.go
	go build -o bin/worker ./cmd/worker
