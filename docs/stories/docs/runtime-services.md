# Runtime Services Documentation Fix Story

## 📋 Objective

Validate and correct the Runtime Services documentation (`src/docs/platform-development-guide/services/runtime-services.md`) to accurately reflect the actual Go microservices implementation for bot execution, backtesting, and scheduling.

## 🎯 Goal

Ensure the Runtime Services documentation accurately describes:
- Bot Runner architecture and implementation
- Backtest Runner design and functionality
- Bot Scheduler implementation
- Master-worker patterns and implementation
- gRPC service definitions and communication
- NATS event handling
- Docker container isolation and security
- Resource management and monitoring
- Actual project structure

## 🔍 Validation Checklist

### 1. Technology Stack Verification
- [ ] Verify Go version from go.mod files
- [ ] Confirm gRPC library version
- [ ] Check NATS client version
- [ ] Verify MongoDB driver version
- [ ] Check MinIO SDK version
- [ ] Document Docker API client version
- [ ] Verify any additional dependencies

**Locations to check**:
- `services/bot-runner/go.mod`
- `services/backtest-runner/go.mod`
- `services/bot-scheduler/go.mod`

### 2. Service Architecture Validation

#### Bot Runner Service
- [ ] Verify service exists and structure
- [ ] Document actual responsibilities
- [ ] Check main.go entry point
- [ ] Verify master-worker implementation
- [ ] Document job queue implementation
- [ ] Check worker pool management
- [ ] Verify container creation logic

**Location to check**: `services/bot-runner/`

#### Backtest Runner Service
- [ ] Verify service exists and structure
- [ ] Document backtest execution flow
- [ ] Check historical data handling
- [ ] Verify result computation
- [ ] Document performance analytics
- [ ] Check result storage

**Location to check**: `services/backtest-runner/`

#### Bot Scheduler Service
- [ ] Verify service exists and structure
- [ ] Document cron implementation
- [ ] Check job scheduling logic
- [ ] Verify trigger mechanisms
- [ ] Document recurring job management

**Location to check**: `services/bot-scheduler/`

### 3. gRPC Service Definitions
- [ ] Verify proto file locations
- [ ] Document actual service definitions
- [ ] Check message types and structures
- [ ] Verify RPC method signatures
- [ ] Document streaming implementations
- [ ] Check error handling patterns

**Locations to check**:
- `services/*/proto/`
- `services/*/pb/` (generated code)
- Proto definitions for each service

### 4. NATS Integration
- [ ] Document NATS connection setup
- [ ] List actual subjects being published to
- [ ] Document subscribed subjects
- [ ] Verify event payload structures
- [ ] Check error handling for events
- [ ] Document JetStream usage (if any)

**Locations to check**:
- `services/*/internal/nats/`
- `services/*/internal/messaging/`
- Event handler implementations

### 5. Docker Container Management
- [ ] Verify Docker API client usage
- [ ] Document container creation process
- [ ] Check image pulling logic
- [ ] Verify volume mounting patterns
- [ ] Document network isolation
- [ ] Check resource limits implementation
- [ ] Verify container cleanup

**Locations to check**:
- `services/*/internal/docker/`
- `services/*/internal/container/`
- Container management code

### 6. Resource Management
- [ ] Document CPU limit implementation
- [ ] Check memory limit enforcement
- [ ] Verify disk I/O restrictions
- [ ] Document timeout mechanisms
- [ ] Check resource monitoring
- [ ] Verify cleanup and recycling

**Locations to check**:
- Container configuration code
- Resource monitoring implementations

### 7. Security & Isolation
- [ ] Verify security options (seccomp, apparmor)
- [ ] Document non-root user enforcement
- [ ] Check read-only filesystem
- [ ] Verify network isolation
- [ ] Document tmpfs configuration
- [ ] Check capability restrictions

**Locations to check**:
- Security configuration in container creation
- Isolation logic

### 8. MongoDB Integration
- [ ] Verify MongoDB connection
- [ ] Document collections being used
- [ ] Check data models
- [ ] Verify query patterns
- [ ] Document state persistence
- [ ] Check index definitions

**Locations to check**:
- `services/*/internal/storage/`
- `services/*/internal/db/`
- MongoDB-related code

### 9. MinIO Integration
- [ ] Verify MinIO client setup
- [ ] Document bucket usage
- [ ] Check file upload/download
- [ ] Verify path conventions
- [ ] Document error handling

**Locations to check**:
- `services/*/internal/storage/`
- MinIO client code

### 10. Configuration & Environment
- [ ] List all environment variables
- [ ] Document configuration files
- [ ] Verify default values
- [ ] Check configuration validation
- [ ] Document .env.example files

**Locations to check**:
- `services/*/.env.example`
- `services/*/internal/config/`
- Configuration loading code

### 11. Monitoring & Health Checks
- [ ] Verify health check endpoints
- [ ] Document metrics being collected
- [ ] Check logging implementation
- [ ] Verify tracing setup (if any)
- [ ] Document alerting patterns

**Locations to check**:
- `services/*/internal/health/`
- `services/*/internal/metrics/`
- Monitoring code

### 12. Project Structure
- [ ] Verify folder structure for each service
- [ ] Document cmd/ entry points
- [ ] Check internal/ package organization
- [ ] Verify proto/ and pb/ structure
- [ ] Document tests location

**Locations to check**:
- Each service's root directory

## 🐛 Known Issues to Address

### Issues Identified
1. **Service Separation**: Doc may combine services that are actually separate
2. **gRPC Definitions**: Proto definitions may be generic or outdated
3. **Master-Worker**: Implementation details may not match actual code
4. **Container Isolation**: Security measures may be incomplete or inaccurate
5. **NATS Events**: Event names and payloads may not match
6. **Generic Code**: Replace with actual Go code from services
7. **Missing Services**: May not document all runtime services that exist

### Sections Needing Verification
- **Service Architecture diagram**: Verify actual service boundaries
- **Technology Stack table**: Update with accurate versions
- **gRPC service definitions**: Replace with actual proto code
- **NATS event handling**: Document actual subjects and payloads
- **Container security**: Verify actual security configuration
- **Code examples**: Replace with real Go code
- **Configuration section**: Update with actual environment variables

## 📝 Tasks to Complete

### Phase 1: Codebase Analysis
1. Identify all runtime services in the codebase
2. Review go.mod files for dependency versions
3. Examine proto files for gRPC definitions
4. Document master-worker implementations
5. Review NATS integration code
6. Analyze Docker container creation logic
7. Document security and isolation measures
8. Review MongoDB and MinIO integrations
9. Document configuration and environment setup

### Phase 2: Documentation Updates
1. Update Technology Stack with accurate versions
2. Clarify service boundaries (separate vs combined)
3. Document each service's specific responsibilities
4. Update gRPC definitions with actual proto code
5. Document NATS event patterns accurately
6. Update container security configuration
7. Replace code examples with real implementations
8. Update configuration section
9. Document monitoring and health checks
10. Update deployment examples

### Phase 3: Validation
1. Cross-reference proto files with documentation
2. Verify container security measures
3. Test gRPC service calls (if possible)
4. Validate NATS event subjects
5. Check configuration examples
6. Verify build and deployment process

## ✅ Success Criteria

- [ ] All runtime services are identified and documented
- [ ] Technology versions match go.mod files
- [ ] gRPC service definitions are accurate (from proto files)
- [ ] Master-worker patterns match implementation
- [ ] NATS events accurately reflect actual subjects
- [ ] Container isolation is correctly documented
- [ ] Resource limits match actual configuration
- [ ] Security measures are accurate and complete
- [ ] Code examples are from actual codebase
- [ ] Configuration is complete and correct
- [ ] Project structure matches actual folders
- [ ] No generic or placeholder content remains

## 📚 Reference Materials

### Codebase Locations
- **Bot Runner**: `services/bot-runner/`
- **Backtest Runner**: `services/backtest-runner/`
- **Bot Scheduler**: `services/bot-scheduler/`
- **Proto Files**: `services/*/proto/`
- **Generated Code**: `services/*/pb/`

### Original Story Reference
- **Main Story**: `stories/infrastructure.md` (lines 119-142)

### Documentation File
- **Target File**: `src/docs/platform-development-guide/services/runtime-services.md`

## 🔗 Related Stories
- API Server documentation fix (for orchestration)
- Data Architecture documentation (for MongoDB/MinIO)
- Docker Deployment documentation
- Security Analyzer documentation (for bot validation)

---

**Priority**: High
**Estimated Effort**: 6-8 hours
**Dependencies**: Access to all runtime service codebases, proto files
