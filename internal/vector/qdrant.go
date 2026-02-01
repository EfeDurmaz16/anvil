package vector

import (
	"context"
	"fmt"

	pb "github.com/qdrant/go-client/qdrant"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

// QdrantRepository implements Repository using Qdrant.
type QdrantRepository struct {
	conn       *grpc.ClientConn
	points     pb.PointsClient
	collection string
}

// NewQdrant creates a Qdrant-backed repository.
func NewQdrant(ctx context.Context, host string, port int, collection string) (*QdrantRepository, error) {
	addr := fmt.Sprintf("%s:%d", host, port)
	conn, err := grpc.NewClient(addr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		return nil, fmt.Errorf("qdrant connect: %w", err)
	}
	return &QdrantRepository{
		conn:       conn,
		points:     pb.NewPointsClient(conn),
		collection: collection,
	}, nil
}

func (r *QdrantRepository) Upsert(ctx context.Context, docs []Document) error {
	points := make([]*pb.PointStruct, len(docs))
	for i, d := range docs {
		payload := map[string]*pb.Value{
			"content": {Kind: &pb.Value_StringValue{StringValue: d.Content}},
		}
		for k, v := range d.Metadata {
			payload[k] = &pb.Value{Kind: &pb.Value_StringValue{StringValue: v}}
		}
		points[i] = &pb.PointStruct{
			Id:      &pb.PointId{PointIdOptions: &pb.PointId_Uuid{Uuid: d.ID}},
			Vectors: &pb.Vectors{VectorsOptions: &pb.Vectors_Vector{Vector: &pb.Vector{Data: d.Vector}}},
			Payload: payload,
		}
	}

	_, err := r.points.Upsert(ctx, &pb.UpsertPoints{
		CollectionName: r.collection,
		Points:         points,
	})
	return err
}

func (r *QdrantRepository) Search(ctx context.Context, vector []float32, topK int) ([]SearchResult, error) {
	resp, err := r.points.Search(ctx, &pb.SearchPoints{
		CollectionName: r.collection,
		Vector:         vector,
		Limit:          uint64(topK),
		WithPayload:    &pb.WithPayloadSelector{SelectorOptions: &pb.WithPayloadSelector_Enable{Enable: true}},
	})
	if err != nil {
		return nil, err
	}

	results := make([]SearchResult, len(resp.Result))
	for i, pt := range resp.Result {
		content := ""
		meta := make(map[string]string)
		for k, v := range pt.Payload {
			if k == "content" {
				content = v.GetStringValue()
			} else {
				meta[k] = v.GetStringValue()
			}
		}
		results[i] = SearchResult{
			ID:       pt.Id.GetUuid(),
			Score:    pt.Score,
			Content:  content,
			Metadata: meta,
		}
	}
	return results, nil
}

func (r *QdrantRepository) Close() error {
	return r.conn.Close()
}
