package gc

import (
	"context"

	"github.com/minio/minio-go/v7"
)

// minioAdapter wraps a minio.Client to implement MinIOClient.
type minioAdapter struct {
	client *minio.Client
}

// NewMinIOAdapter creates a MinIOClient from a minio.Client.
func NewMinIOAdapter(client *minio.Client) MinIOClient {
	return &minioAdapter{client: client}
}

func (m *minioAdapter) ListObjectNames(ctx context.Context, bucket, prefix string) ([]string, error) {
	var names []string
	for obj := range m.client.ListObjects(ctx, bucket, minio.ListObjectsOptions{
		Prefix:    prefix,
		Recursive: true,
	}) {
		if obj.Err != nil {
			return nil, obj.Err
		}
		names = append(names, obj.Key)
	}
	return names, nil
}

func (m *minioAdapter) RemoveObject(ctx context.Context, bucket, name string) error {
	return m.client.RemoveObject(ctx, bucket, name, minio.RemoveObjectOptions{})
}
