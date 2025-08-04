# Docker Runner Refactoring Plan

## Executive Summary

The current Docker Runner implementation has grown to over 1,400 lines in a single file, handling multiple responsibilities that should be separated for better maintainability, testability, and extensibility. This document outlines a comprehensive refactoring plan to transform the monolithic docker runner into a service-oriented architecture.

## Current State Analysis

### Size & Complexity
- **Main file**: 1,397 lines (`docker_runner.go`)
- **Test file**: 833 lines (`docker_runner_test.go`) 
- **Entrypoints**: 841 lines across 4 files
- **Total**: ~3,071 lines of code

### Current Responsibilities (Too Many!)
1. Container lifecycle management
2. Code download & extraction
3. Docker operations & image management
4. Entrypoint script generation
5. Log collection & streaming
6. MinIO integration & storage
7. Metrics tracking (15+ methods)
8. Runtime validation & mapping

### Problems with Current Design
- **Violation of Single Responsibility Principle**: One struct doing everything
- **Testing Complexity**: Hard to test individual components in isolation
- **Maintainability Issues**: Large files are difficult to navigate and modify
- **Extension Difficulty**: Adding new runtimes or storage backends requires modifying core logic
- **Debugging Challenges**: Issues are hard to isolate to specific functionality
- **Code Reusability**: Components can't be reused in different contexts

## Proposed Architecture: Service-Oriented Design

### Phase 1: Extract Core Services

#### 1.1 Container Service
**File**: `internal/docker-runner/services/container_service.go`  
**Responsibility**: Pure Docker container operations

```go
type ContainerService interface {
    CreateContainer(ctx context.Context, config ContainerConfig) (string, error)
    StartContainer(ctx context.Context, containerID string) error
    StopContainer(ctx context.Context, containerID string) error
    GetContainerStatus(ctx context.Context, containerID string) (*ContainerStatus, error)
    ListContainers(ctx context.Context, filters map[string]string) ([]*ContainerInfo, error)
    GetContainerLogs(ctx context.Context, containerID string, opts LogOptions) (string, error)
    RemoveContainer(ctx context.Context, containerID string) error
    PullImage(ctx context.Context, imageName string) error
}
```

#### 1.2 Code Management Service
**File**: `internal/docker-runner/services/code_service.go`  
**Responsibility**: Bot code download, extraction, and preparation

```go
type CodeService interface {
    DownloadCode(ctx context.Context, executable model.Executable) ([]byte, error)
    ExtractCode(data []byte, destDir string, format CodeFormat) error
    PrepareWorkspace(ctx context.Context, executable model.Executable) (string, error)
    CleanupWorkspace(workspaceDir string) error
    ValidateCodeStructure(workspaceDir string, runtime string) error
}
```

#### 1.3 Storage Service
**File**: `internal/docker-runner/services/storage_service.go`  
**Responsibility**: MinIO operations and file storage

```go
type StorageService interface {
    StoreResults(ctx context.Context, botID string, data []byte) error
    StoreLogs(ctx context.Context, botID string, logs string) error
    RetrieveCode(ctx context.Context, filePath string) ([]byte, error)
    StoreAnalysis(ctx context.Context, botID string, analysis []byte) error
    DeleteFiles(ctx context.Context, prefix string) error
}
```

### Phase 2: Extract Supporting Components

#### 2.1 Entrypoint Manager
**File**: `internal/docker-runner/entrypoints/entrypoint_manager.go`  
**Responsibility**: Centralized entrypoint script generation

```go
type EntrypointManager interface {
    CreateEntrypointScript(executable model.Executable, workspaceDir string) (string, error)
    GetSupportedRuntimes() []string
    ValidateRuntime(runtime string) error
    GetDockerImage(runtime string) string
}
```

#### 2.2 Log Collector Service
**File**: `internal/docker-runner/services/log_collector.go`  
**Responsibility**: Container log collection and streaming

```go
type LogCollector interface {
    StartCollection(containerID string) error
    StopCollection(containerID string) error
    CollectLogs(ctx context.Context, containerID string) (string, error)
    StreamLogs(ctx context.Context, containerID string) (<-chan string, error)
    GetCollectionStatus(containerID string) CollectionStatus
}
```

#### 2.3 Metrics Service
**File**: `internal/docker-runner/services/metrics_service.go`  
**Responsibility**: Performance and operation metrics

```go
type MetricsService interface {
    RecordContainerOperation(operation string, duration time.Duration, success bool)
    RecordLogOperation(operation string, success bool)
    RecordStorageOperation(operation string, size int64, success bool)
    GetMetrics() MetricsSnapshot
    ResetMetrics()
    LogMetricsSummary()
}
```

### Phase 3: Orchestrator Pattern

#### 3.1 Bot Execution Orchestrator
**File**: `internal/docker-runner/orchestrator/bot_orchestrator.go`  
**Responsibility**: Coordinate all services for bot execution

```go
type BotOrchestrator struct {
    containerService  ContainerService
    codeService      CodeService
    storageService   StorageService
    entrypointManager EntrypointManager
    logCollector     LogCollector
    metricsService   MetricsService
    config           Config
}

func (o *BotOrchestrator) ExecuteBot(ctx context.Context, executable model.Executable) (*ExecutionResult, error)
func (o *BotOrchestrator) ExecuteBacktest(ctx context.Context, executable model.Executable) (*ExecutionResult, error)
func (o *BotOrchestrator) StopExecution(ctx context.Context, containerID string) error
```

#### 3.2 Simplified DockerRunner Interface
**File**: `internal/docker-runner/docker_runner.go` (refactored)

```go
type DockerRunner interface {
    // High-level operations (unchanged for backwards compatibility)
    StartContainer(ctx context.Context, executable model.Executable) (*ExecutionResult, error)
    StopContainer(ctx context.Context, containerID string, executable model.Executable) error
    GetContainerStatus(ctx context.Context, containerID string) (*ContainerStatus, error)
    ListManagedContainers(ctx context.Context) ([]*ContainerInfo, error)
    GetContainerLogs(ctx context.Context, containerID string, tail int) (string, error)
    Close() error
}

// Implementation delegates to orchestrator
type dockerRunner struct {
    orchestrator *BotOrchestrator
    // Maintain existing fields for backwards compatibility
}
```

### Phase 4: Enhanced Testing Strategy

#### 4.1 Unit Tests per Service
- `container_service_test.go` - Test pure Docker operations with mock Docker client
- `code_service_test.go` - Test code download/extraction with mock MinIO
- `storage_service_test.go` - Test MinIO operations with testcontainers
- `entrypoint_manager_test.go` - Test script generation logic
- `log_collector_test.go` - Test log collection with mock containers
- `metrics_service_test.go` - Test metrics tracking and reporting

#### 4.2 Integration Tests
- `bot_orchestrator_test.go` - Test service coordination with mocked services
- `docker_runner_integration_test.go` - Test end-to-end flows (keep existing tests)

#### 4.3 Mock Services for Testing
Create mock implementations of each service interface to enable:
- Isolated testing of orchestrator logic
- Fast unit tests without external dependencies
- Reliable CI/CD pipeline execution

### Phase 5: Configuration & Dependency Injection

#### 5.1 Service Configuration
**File**: `internal/docker-runner/config/config.go`

```go
type Config struct {
    Docker   DockerConfig
    MinIO    MinIOConfig
    Logging  LoggingConfig
    Metrics  MetricsConfig
    Runtime  RuntimeConfig
}

type DockerConfig struct {
    Host           string
    APIVersion     string
    TLSVerify      bool
    CertPath       string
    MemoryLimit    int64
    CPUShares      int64
}

type MinIOConfig struct {
    Endpoint        string
    AccessKey       string
    SecretKey       string
    UseSSL          bool
    BucketName      string
    Region          string
}
```

#### 5.2 Service Factory
**File**: `internal/docker-runner/factory/service_factory.go`

```go
type ServiceFactory interface {
    CreateContainerService(config DockerConfig) (ContainerService, error)
    CreateCodeService(config CodeConfig) (CodeService, error)
    CreateStorageService(config MinIOConfig) (StorageService, error)
    CreateEntrypointManager() (EntrypointManager, error)
    CreateLogCollector(config LoggingConfig) (LogCollector, error)
    CreateMetricsService(config MetricsConfig) (MetricsService, error)
}
```

## Migration Strategy

### Backwards Compatibility
1. **Keep Existing Interface**: The main `DockerRunner` interface remains unchanged
2. **Internal Delegation**: New implementation delegates to orchestrator internally
3. **Gradual Migration**: Extract one service at a time
4. **Feature Flags**: Control rollout of new architecture components
5. **Comprehensive Testing**: Ensure no regression during migration

### Migration Phases

#### Phase 1: Service Extraction
1. Extract `ContainerService` with existing Docker logic
2. Extract `CodeService` with download/extraction logic
3. Extract `StorageService` with MinIO operations
4. Maintain existing `dockerRunner` struct, delegate to services

#### Phase 2: Component Separation  
1. Extract `EntrypointManager` from existing entrypoint code
2. Extract `LogCollector` from existing log collection logic
3. Extract `MetricsService` from existing metrics methods
4. Refactor entrypoint packages for better organization

#### Phase 3: Orchestrator Implementation
1. Create `BotOrchestrator` that coordinates all services
2. Refactor `dockerRunner` to delegate to orchestrator
3. Implement proper error handling and context propagation
4. Add comprehensive logging and debugging support

#### Phase 4: Testing Enhancement
1. Create comprehensive unit tests for each service
2. Implement mock services for testing
3. Enhance integration tests with better coverage
4. Add performance benchmarks for critical paths

#### Phase 5: Configuration & Polish
1. Implement centralized configuration management
2. Add service factory for dependency injection
3. Improve error messages and debugging information
4. Add comprehensive documentation and examples

## Benefits of This Refactoring

### Code Quality
- **Single Responsibility**: Each service has one clear purpose
- **Separation of Concerns**: Logic properly isolated by functionality
- **Reduced Complexity**: Smaller, focused code files (~200-300 lines each)
- **Better Error Handling**: Specific error types for each service

### Testing & Debugging
- **Testability**: Services can be tested independently with mocks
- **Isolation**: Issues can be traced to specific services
- **Mocking**: Easy to mock individual services for testing
- **Coverage**: Better test coverage through focused testing

### Maintainability & Extensibility
- **Modularity**: Changes isolated to specific services
- **Extensibility**: Easy to add new runtimes, storage backends, etc.
- **Reusability**: Services can be used in different contexts
- **Documentation**: Clear interfaces document expected behavior

### Performance
- **Optimization**: Services can be optimized independently
- **Caching**: Service-level caching strategies
- **Resource Management**: Better control over resource usage
- **Monitoring**: Service-specific metrics and monitoring

## Risk Mitigation

### Technical Risks
- **Regression Risk**: Mitigated by comprehensive testing and gradual migration
- **Performance Risk**: Mitigated by benchmarking and performance testing
- **Complexity Risk**: Mitigated by clear interfaces and documentation

### Operational Risks
- **Deployment Risk**: Mitigated by backwards compatibility and feature flags
- **Integration Risk**: Mitigated by maintaining existing interfaces
- **Monitoring Risk**: Mitigated by enhanced metrics and logging

## Success Metrics

### Code Quality Metrics
- Lines of code per file reduced to <300
- Cyclomatic complexity reduced significantly
- Test coverage increased to >90%
- Number of responsibilities per class/struct reduced to 1-2

### Performance Metrics
- Container startup time maintained or improved
- Memory usage optimized through better resource management
- Error rates maintained or reduced
- Log collection performance improved

### Maintainability Metrics
- Time to add new runtime reduced significantly
- Time to debug issues reduced through better isolation
- Code review time reduced through smaller, focused changes
- Developer onboarding time reduced through clearer architecture

## Post-Refactoring Architecture

```
internal/docker-runner/
├── PLAN.md                           # This document
├── docker_runner.go                  # Main interface (simplified)
├── docker_runner_test.go            # Integration tests
├── config/
│   ├── config.go                    # Configuration structures
│   └── config_test.go               # Configuration tests
├── services/
│   ├── container_service.go         # Docker operations
│   ├── container_service_test.go    # Container service tests
│   ├── code_service.go              # Code management
│   ├── code_service_test.go         # Code service tests
│   ├── storage_service.go           # MinIO operations
│   ├── storage_service_test.go      # Storage service tests
│   ├── log_collector.go             # Log collection
│   ├── log_collector_test.go        # Log collector tests
│   ├── metrics_service.go           # Metrics tracking
│   └── metrics_service_test.go      # Metrics service tests
├── orchestrator/
│   ├── bot_orchestrator.go          # Service coordination
│   └── bot_orchestrator_test.go     # Orchestrator tests
├── factory/
│   ├── service_factory.go           # Dependency injection
│   └── service_factory_test.go      # Factory tests
├── mocks/                           # Mock implementations
│   ├── mock_container_service.go
│   ├── mock_code_service.go
│   ├── mock_storage_service.go
│   └── ...
└── entrypoints/                     # Enhanced entrypoint management
    ├── entrypoint_manager.go        # Centralized manager
    ├── entrypoint_manager_test.go   # Manager tests
    ├── bash_entrypoint_factory.go   # Existing (refactored)
    ├── code_entrypoint_factory.go   # Existing (refactored)
    ├── python311.go                 # Existing (refactored)
    └── nodejs20.go                  # Existing (refactored)
```

This refactoring will transform the Docker Runner from a monolithic component into a well-structured, maintainable, and extensible service-oriented architecture while maintaining full backwards compatibility.