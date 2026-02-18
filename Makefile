.PHONY: build test lint clean setup docker-build integration-test fmt coverage

build:
	go build ./...

test:
	go test ./... -v -count=1

lint:
	golangci-lint run ./...

clean:
	rm -f bin/anvil bin/worker

setup:
	git config core.hooksPath .githooks

bin/anvil: cmd/anvil/main.go
	go build -o bin/anvil ./cmd/anvil

bin/worker: cmd/worker/main.go
	go build -o bin/worker ./cmd/worker

docker-build:
	docker build -t anvil:latest .

integration-test:
	go test -tags=integration -race ./...

fmt:
	gofmt -w .

coverage:
	go test -coverprofile=coverage.out ./... && go tool cover -html=coverage.out
