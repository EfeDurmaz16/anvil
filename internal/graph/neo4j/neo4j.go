package neo4j

import (
	"context"
	"fmt"

	"github.com/efebarandurmaz/anvil/internal/graph"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/neo4j/neo4j-go-driver/v5/neo4j"
)

// Repository implements graph.Repository using Neo4j.
type Neo4jRepository struct {
	driver neo4j.DriverWithContext
}

// New creates a Neo4j-backed repository.
func NewNeo4j(ctx context.Context, uri, username, password string) (*Neo4jRepository, error) {
	driver, err := neo4j.NewDriverWithContext(uri, neo4j.BasicAuth(username, password, ""))
	if err != nil {
		return nil, fmt.Errorf("neo4j driver: %w", err)
	}
	if err := driver.VerifyConnectivity(ctx); err != nil {
		return nil, fmt.Errorf("neo4j connectivity: %w", err)
	}
	return &Neo4jRepository{driver: driver}, nil
}

func (r *Neo4jRepository) StoreGraph(ctx context.Context, g *ir.SemanticGraph) error {
	session := r.driver.NewSession(ctx, neo4j.SessionConfig{})
	defer session.Close(ctx)

	for _, mod := range g.Modules {
		_, err := session.ExecuteWrite(ctx, func(tx neo4j.ManagedTransaction) (any, error) {
			_, err := tx.Run(ctx,
				"MERGE (m:Module {name: $name}) SET m.path = $path, m.language = $lang",
				map[string]any{"name": mod.Name, "path": mod.Path, "lang": mod.Language})
			if err != nil {
				return nil, err
			}
			for _, fn := range mod.Functions {
				_, err := tx.Run(ctx,
					"MERGE (f:Function {name: $name}) "+
						"MERGE (m:Module {name: $mod}) "+
						"MERGE (m)-[:CONTAINS]->(f)",
					map[string]any{"name": fn.Name, "mod": mod.Name})
				if err != nil {
					return nil, err
				}
			}
			return nil, nil
		})
		if err != nil {
			return fmt.Errorf("store module %s: %w", mod.Name, err)
		}
	}

	if g.CallGraph != nil {
		_, err := session.ExecuteWrite(ctx, func(tx neo4j.ManagedTransaction) (any, error) {
			for _, e := range g.CallGraph.Edges {
				_, err := tx.Run(ctx,
					"MERGE (a:Function {name: $caller}) "+
						"MERGE (b:Function {name: $callee}) "+
						"MERGE (a)-[:CALLS]->(b)",
					map[string]any{"caller": e.Caller, "callee": e.Callee})
				if err != nil {
					return nil, err
				}
			}
			return nil, nil
		})
		if err != nil {
			return fmt.Errorf("store call graph: %w", err)
		}
	}
	return nil
}

func (r *Neo4jRepository) LoadGraph(ctx context.Context, projectID string) (*ir.SemanticGraph, error) {
	_ = projectID
	session := r.driver.NewSession(ctx, neo4j.SessionConfig{})
	defer session.Close(ctx)

	result, err := session.ExecuteRead(ctx, func(tx neo4j.ManagedTransaction) (any, error) {
		records, err := tx.Run(ctx,
			"MATCH (m:Module) OPTIONAL MATCH (m)-[:CONTAINS]->(f:Function) RETURN m.name, m.path, m.language, collect(f.name) as fns",
			nil)
		if err != nil {
			return nil, err
		}

		var modules []*ir.Module
		for records.Next(ctx) {
			rec := records.Record()
			name, _ := rec.Get("m.name")
			path, _ := rec.Get("m.path")
			lang, _ := rec.Get("m.language")
			fns, _ := rec.Get("fns")

			mod := &ir.Module{
				Name:     name.(string),
				Path:     path.(string),
				Language: lang.(string),
			}
			for _, fn := range fns.([]any) {
				if fn != nil {
					mod.Functions = append(mod.Functions, &ir.Function{Name: fn.(string)})
				}
			}
			modules = append(modules, mod)
		}
		return &ir.SemanticGraph{Modules: modules}, nil
	})
	if err != nil {
		return nil, err
	}
	return result.(*ir.SemanticGraph), nil
}

func (r *Neo4jRepository) QueryCallees(ctx context.Context, functionName string) ([]string, error) {
	session := r.driver.NewSession(ctx, neo4j.SessionConfig{})
	defer session.Close(ctx)

	result, err := session.ExecuteRead(ctx, func(tx neo4j.ManagedTransaction) (any, error) {
		records, err := tx.Run(ctx,
			"MATCH (:Function {name: $name})-[:CALLS]->(callee:Function) RETURN callee.name",
			map[string]any{"name": functionName})
		if err != nil {
			return nil, err
		}
		var names []string
		for records.Next(ctx) {
			n, _ := records.Record().Get("callee.name")
			names = append(names, n.(string))
		}
		return names, nil
	})
	if err != nil {
		return nil, err
	}
	return result.([]string), nil
}

func (r *Neo4jRepository) Close(ctx context.Context) error {
	return r.driver.Close(ctx)
}

var _ graph.Repository = (*Neo4jRepository)(nil)
