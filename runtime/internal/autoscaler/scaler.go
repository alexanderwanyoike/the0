package autoscaler

import (
	"context"
	"fmt"
	"os"
	"strconv"
	"time"

	"runtime/internal/metrics"
	"runtime/internal/util"

	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	mongoOptions "go.mongodb.org/mongo-driver/mongo/options"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/client-go/kubernetes"
	"k8s.io/client-go/rest"
)

// Scaler implements native Kubernetes scaling without HPA
type Scaler struct {
	client         kubernetes.Interface
	mongoClient    *mongo.Client
	namespace      string
	deployment     string
	minReplicas    int32
	maxReplicas    int32
	lastScaleTime  time.Time
	cooldownPeriod time.Duration
	dbName         string
	collectionName string
}

// NewScaler creates a new scaler
func NewScaler(
	mongoUri string,
	dbName string,
	collectionName string,
) (*Scaler, error) {
	config, err := rest.InClusterConfig()
	if err != nil {
		return nil, fmt.Errorf("failed to get in-cluster config: %v", err)
	}

	client, err := kubernetes.NewForConfig(config)
	if err != nil {
		return nil, fmt.Errorf("failed to create kubernetes client: %v", err)
	}

	if mongoUri == "" {
		return nil, fmt.Errorf("MONGO_URI environment variable is not set")
	}

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	mongoClient, err := mongo.Connect(ctx, mongoOptions.Client().ApplyURI(mongoUri))
	if err != nil {
		return nil, fmt.Errorf("failed to connect to MongoDB: %v", err)
	}

	// Verify MongoDB connection
	if err := mongoClient.Ping(ctx, nil); err != nil {
		return nil, fmt.Errorf("failed to ping MongoDB: %v", err)
	}

	return &Scaler{
		client:         client,
		mongoClient:    mongoClient,
		namespace:      getEnvOrDefault("NAMESPACE", "default"),
		deployment:     getEnvOrDefault("DEPLOYMENT_NAME", "runtime-worker"), // Only scale workers
		minReplicas:    getEnvInt32("MIN_REPLICAS", 3),
		maxReplicas:    getEnvInt32("MAX_REPLICAS", 4),
		dbName:         dbName,
		collectionName: collectionName,
		cooldownPeriod: 10 * time.Second, // More aggressive scaling
	}, nil
}

// Start begins the autoscaling loop
func (s *Scaler) Start(ctx context.Context) {
	ticker := time.NewTicker(15 * time.Second) // Check every 15 seconds
	defer ticker.Stop()

	util.LogMaster("Starting autoscaler (min: %d, max: %d)", s.minReplicas, s.maxReplicas)

	for {
		select {
		case <-ctx.Done():
			util.LogMaster("Autoscaler stopped")
			return
		case <-ticker.C:
			if err := s.checkAndScale(); err != nil {
				util.LogMaster("Error during scaling check: %v", err)
			}
		}
	}
}

// checkAndScale checks metrics and scales the deployment if needed
func (s *Scaler) checkAndScale() error {
	metrics := s.GetSegmentMetrics(context.TODO())

	// Get current deployment
	deployment, err := s.client.AppsV1().Deployments(s.namespace).Get(
		context.Background(),
		s.deployment,
		metav1.GetOptions{},
	)
	if err != nil {
		return fmt.Errorf("failed to get deployment: %v", err)
	}

	currentReplicas := *deployment.Spec.Replicas
	desiredReplicas := s.calculateDesiredReplicas(metrics, currentReplicas)

	// Check cooldown period
	if time.Since(s.lastScaleTime) < s.cooldownPeriod {
		return nil // Skip scaling during cooldown
	}

	// Only scale if needed
	if desiredReplicas != currentReplicas {
		target := int32(metrics.TotalSegments)
		direction := "down"
		if desiredReplicas > currentReplicas {
			direction = "up"
		}
		util.LogMaster("Scaling %s from %d to %d replicas (total_segments: %d, target: %d, gradual_scaling: +/-1)",
			direction, currentReplicas, desiredReplicas, metrics.TotalSegments, target)

		deployment.Spec.Replicas = &desiredReplicas
		_, err := s.client.AppsV1().Deployments(s.namespace).Update(
			context.Background(),
			deployment,
			metav1.UpdateOptions{},
		)
		if err != nil {
			return fmt.Errorf("failed to update deployment: %v", err)
		}

		s.lastScaleTime = time.Now()
		util.LogMaster("Scaling completed successfully")
	}

	return nil
}

// GetSegmentMetrics returns current segment metrics from MongoDB
func (s *Scaler) GetSegmentMetrics(ctx context.Context) metrics.SegmentMetrics {
	// Get total available segments from MongoDB
	totalSegments := 0
	if s.mongoClient != nil {
		partitionsCollection := s.mongoClient.Database(s.dbName).Collection("partitions")
		count, err := partitionsCollection.CountDocuments(ctx, bson.M{"bot_count": bson.M{"$gt": 0}})
		if err == nil {
			totalSegments = int(count)
		}
	}

	return metrics.SegmentMetrics{
		TotalSegments: totalSegments,
	}
}

// calculateDesiredReplicas determines the ideal number of replicas
func (s *Scaler) calculateDesiredReplicas(metrics metrics.SegmentMetrics, current int32) int32 {
	// 1 worker per segment, master is separate deployment
	target := int32(metrics.TotalSegments)

	var desired int32
	if target > current {
		desired = current + 1
	} else if target < current {
		desired = current - 1
	} else {
		desired = current // No change needed
	}

	// Apply min/max bounds after gradual scaling
	if desired < s.minReplicas {
		desired = s.minReplicas
	}
	if desired > s.maxReplicas {
		desired = s.maxReplicas
	}

	return desired
}

func getEnvOrDefault(key, defaultValue string) string {
	if value := os.Getenv(key); value != "" {
		return value
	}
	return defaultValue
}

// getEnvInt32 gets an int32 value from environment variable with default
func getEnvInt32(key string, defaultValue int32) int32 {
	if value := os.Getenv(key); value != "" {
		if parsed, err := strconv.ParseInt(value, 10, 32); err == nil {
			return int32(parsed)
		}
		util.LogMaster("Warning: Invalid value for %s: %s, using default %d", key, value, defaultValue)
	}
	return defaultValue
}
