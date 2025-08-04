package server

import (
	"context"
	"github.com/gin-gonic/gin"
	"google.golang.org/grpc"
	"net"
	"net/http"
	"runtime/internal/autoscaler"
	"runtime/internal/util"
	"time"
	pb "runtime/pb"
)

type Master struct {
	api          *gin.Engine
	httpServer   *http.Server
	ln           net.Listener
	svr          *grpc.Server
	workerSvr    *WorkerServiceGrpcServer
	scaler       *autoscaler.Scaler
	scalerCtx    context.Context
	scalerCancel context.CancelFunc
	Started      bool
}

func (master *Master) Init(
	mongoUri string,
	dbName string,
	collectionName string,
	address string,
) (err error) {
	master.ln, err = net.Listen("tcp", address)
	if err != nil {
		return err
	}

	// Create a new gRPC server and register the worker service
	master.svr = grpc.NewServer()
	master.workerSvr, err = GetWorkerServiceGrpcServer(
		mongoUri,
		dbName,
		collectionName,
	)
	if err != nil {
		return err
	}
	pb.RegisterWorkerServer(master.svr, master.workerSvr)

	master.api = gin.Default()

	// Health check endpoint (required for Kubernetes probes)
	master.api.GET("/healthz", func(c *gin.Context) {
		c.JSON(200, gin.H{"status": "ok"})
	})

	// Initialize HTTP server
	master.httpServer = &http.Server{
		Addr:    ":8080",
		Handler: master.api,
	}

	// Initialize native Kubernetes autoscaler
	master.scaler, err = autoscaler.NewScaler(
		mongoUri,
		dbName,
		collectionName,
	)
	if err != nil {
		// Log error but don't fail - autoscaler is optional
		// This allows the system to work without Kubernetes permissions
		master.scaler = nil
	}

	return nil
}

func (master *Master) Start() {
	// Start gRPC server
	go func() {
		util.LogMaster("Starting gRPC server on :50051")
		if err := master.svr.Serve(master.ln); err != nil {
			util.LogMaster("gRPC server error: %v", err)
		}
	}()

	// Start native autoscaler if available
	if master.scaler != nil {
		master.scalerCtx, master.scalerCancel = context.WithCancel(context.Background())
		go master.scaler.Start(master.scalerCtx)
	}

	master.Started = true
	util.LogMaster("Starting HTTP server on :8080")
	if err := master.httpServer.ListenAndServe(); err != nil && err != http.ErrServerClosed {
		util.LogMaster("HTTP server error: %v", err)
	}
}

func (master *Master) Stop() {
	// Stop autoscaler
	if master.scalerCancel != nil {
		master.scalerCancel()
	}

	// Stop HTTP server
	if master.httpServer != nil {
		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		if err := master.httpServer.Shutdown(ctx); err != nil {
			util.LogMaster("HTTP server shutdown error: %v", err)
		}
	}

	if master.svr != nil {
		master.svr.Stop()
	}
	if master.ln != nil {
		_ = master.ln.Close()
	}
	if master.workerSvr != nil {
		master.workerSvr.Stop()
	}
	master.Started = false
}

func NewMaster(
	mongoUri string,
	dbName string,
	collectionName string,
	address string,
) (*Master, error) {
	master := &Master{}
	if err := master.Init(
		mongoUri,
		dbName,
		collectionName,
		address,
	); err != nil {
		return nil, err
	}
	return master, nil
}
