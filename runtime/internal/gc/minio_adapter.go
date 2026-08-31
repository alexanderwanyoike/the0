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

// bucketAPI is the subset of minio.Client that ensureBucket needs, split out
// so the concurrent-creation race can be tested without a live server.
type bucketAPI interface {
	BucketExists(ctx context.Context, bucket string) (bool, error)
	MakeBucket(ctx context.Context, bucket string, opts minio.MakeBucketOptions) error
}

func ensureBucket(ctx context.Context, api bucketAPI, bucket string) error {
	exists, err := api.BucketExists(ctx, bucket)
	if err != nil {
		return err
	}
	if exists {
		return nil
	}
	if err := api.MakeBucket(ctx, bucket, minio.MakeBucketOptions{}); err != nil {
		// Several services create these buckets lazily, so on a fresh stack
		// another one can win the create between our exists check and here.
		// The bucket existing is all we need; only fail if it still doesn't.
		if exists, checkErr := api.BucketExists(ctx, bucket); checkErr == nil && exists {
			return nil
		}
		return err
	}
	return nil
}

func (m *minioAdapter) EnsureBucket(ctx context.Context, bucket string) error {
	return ensureBucket(ctx, m.client, bucket)
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

func (m *minioAdapter) ListObjectsWithInfo(ctx context.Context, bucket, prefix string) ([]ObjectInfo, error) {
	var objects []ObjectInfo
	for obj := range m.client.ListObjects(ctx, bucket, minio.ListObjectsOptions{
		Prefix:    prefix,
		Recursive: true,
	}) {
		if obj.Err != nil {
			return nil, obj.Err
		}
		objects = append(objects, ObjectInfo{
			Name:         obj.Key,
			LastModified: obj.LastModified,
		})
	}
	return objects, nil
}

func (m *minioAdapter) RemoveObject(ctx context.Context, bucket, name string) error {
	return m.client.RemoveObject(ctx, bucket, name, minio.RemoveObjectOptions{})
}

func (m *minioAdapter) ListIncompleteUploads(ctx context.Context, bucket, prefix string) ([]IncompleteUploadInfo, error) {
	var uploads []IncompleteUploadInfo
	for u := range m.client.ListIncompleteUploads(ctx, bucket, prefix, true) {
		if u.Err != nil {
			return nil, u.Err
		}
		uploads = append(uploads, IncompleteUploadInfo{
			Key:       u.Key,
			UploadID:  u.UploadID,
			Initiated: u.Initiated,
		})
	}
	return uploads, nil
}

func (m *minioAdapter) AbortIncompleteUpload(ctx context.Context, bucket, key, uploadID string) error {
	core := minio.Core{Client: m.client}
	return core.AbortMultipartUpload(ctx, bucket, key, uploadID)
}
