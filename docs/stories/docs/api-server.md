# API Server Documentation Fix Story

## 📋 Objective

Validate and correct the API Server documentation (`src/docs/platform-development-guide/services/api-server.md`) to accurately reflect the actual NestJS implementation in the the0 codebase.

## 🎯 Goal

Ensure the API Server documentation accurately describes:
- The actual REST API endpoints and routes
- Real database schemas (Drizzle ORM)
- Actual NATS event handlers and message patterns
- Current authentication and authorization implementation
- Accurate technology versions and dependencies
- Real service architecture and module structure

## 🔍 Validation Checklist

### 1. Technology Stack Verification
- [ ] Verify NestJS version in package.json
- [ ] Confirm TypeScript version
- [ ] Check PostgreSQL version from docker-compose or deployment configs
- [ ] Verify MongoDB version and usage
- [ ] Confirm Drizzle ORM version
- [ ] Check NATS client version
- [ ] Verify MinIO client library version
- [ ] Confirm JWT implementation library (e.g., @nestjs/jwt)
- [ ] Check Ajv version for schema validation

**Location to check**: `services/the0-api/package.json`

### 2. API Endpoints Validation
- [ ] Review actual NestJS controllers to list real endpoints
- [ ] Verify authentication endpoints (login, register, refresh, logout)
- [ ] Check bot management endpoints (CRUD operations)
- [ ] Verify custom bot endpoints
- [ ] Check backtest endpoints
- [ ] Verify API key management endpoints
- [ ] Document actual request/response formats
- [ ] Confirm authentication requirements (JWT, API key)

**Locations to check**:
- `services/the0-api/src/modules/auth/controllers/`
- `services/the0-api/src/modules/bot/controllers/`
- `services/the0-api/src/modules/custom-bot/controllers/`
- `services/the0-api/src/modules/backtest/controllers/`
- `services/the0-api/src/modules/api-key/controllers/`

### 3. Database Schema Verification
- [ ] Review actual Drizzle ORM schema definitions
- [ ] Verify users table structure
- [ ] Check bots table structure
- [ ] Verify custom_bots table
- [ ] Check backtests table
- [ ] Verify api_keys table
- [ ] Document actual relationships between tables
- [ ] Verify indexes and constraints
- [ ] Check for any additional tables not documented

**Locations to check**:
- `services/the0-api/src/db/schema/`
- `services/the0-api/src/db/migrations/`

### 4. NATS Event Handling
- [ ] List actual NATS subjects being published to
- [ ] Document events being subscribed to
- [ ] Verify event payload structures
- [ ] Check error handling for event publishing
- [ ] Document retry logic and dead letter queues
- [ ] Verify correlation IDs and tracing

**Locations to check**:
- `services/the0-api/src/modules/*/services/` (service files with NATS logic)
- `services/the0-api/src/nats/` (if exists)

### 5. Authentication & Authorization
- [ ] Verify JWT implementation details
- [ ] Check token generation and validation
- [ ] Verify refresh token mechanism
- [ ] Document API key generation and validation
- [ ] Check role-based access control (if implemented)
- [ ] Verify password hashing algorithm
- [ ] Document session management

**Locations to check**:
- `services/the0-api/src/modules/auth/`
- `services/the0-api/src/guards/`
- `services/the0-api/src/strategies/`

### 6. MinIO Integration
- [ ] Verify MinIO client configuration
- [ ] Document actual bucket names
- [ ] Check file upload/download patterns
- [ ] Verify signed URL generation
- [ ] Document file naming conventions
- [ ] Check error handling for storage operations

**Locations to check**:
- `services/the0-api/src/storage/`
- `services/the0-api/src/modules/custom-bot/services/`

### 7. Configuration & Environment
- [ ] List all environment variables actually used
- [ ] Document required vs optional variables
- [ ] Verify default values
- [ ] Check configuration validation
- [ ] Document .env.example file

**Locations to check**:
- `services/the0-api/.env.example`
- `services/the0-api/src/config/`

### 8. Project Structure
- [ ] Verify actual folder structure
- [ ] Document module organization
- [ ] Check for shared libraries or utilities
- [ ] Verify main.ts bootstrap logic
- [ ] Document middleware and interceptors

**Locations to check**:
- `services/the0-api/src/`

## 🐛 Known Issues to Address

### Issues Identified
1. **Generic Code Examples**: Replace placeholder code with actual implementation snippets from the codebase
2. **Endpoint Documentation**: May not match actual controller routes
3. **Database Schema**: Schema definitions may be outdated or generic
4. **NATS Events**: Event names and payloads may not match actual implementation
5. **Missing Features**: Documentation may omit features that exist in code
6. **Deprecated Features**: Documentation may include features that no longer exist

### Sections Needing Verification
- **Technology Stack table**: Verify all versions
- **REST API Endpoints table**: Compare with actual controllers
- **Event Handling table**: Validate against actual NATS implementation
- **Database schema examples**: Replace with actual Drizzle schemas
- **Code examples**: Replace generic examples with real code snippets
- **Configuration section**: Update with actual environment variables

## 📝 Tasks to Complete

### Phase 1: Codebase Analysis
1. Navigate to API server codebase
2. Review package.json for accurate dependency versions
3. Examine all controller files to document actual endpoints
4. Review schema files for database structure
5. Check NATS integration for event patterns
6. Document authentication implementation

### Phase 2: Documentation Updates
1. Update Technology Stack table with accurate versions
2. Rewrite API Endpoints table with actual routes
3. Update database schema examples with real Drizzle schemas
4. Document actual NATS events and payloads
5. Replace generic code examples with real implementations
6. Update configuration section with actual environment variables
7. Add any missing sections discovered during analysis

### Phase 3: Validation
1. Cross-reference updated documentation with codebase
2. Ensure all code examples are accurate and tested
3. Verify all endpoints are documented
4. Check that architecture diagrams match reality
5. Validate external links and references

## ✅ Success Criteria

- [ ] All technology versions match package.json
- [ ] All API endpoints are accurately documented with correct methods, paths, and auth requirements
- [ ] Database schemas match actual Drizzle ORM definitions
- [ ] NATS events accurately reflect implementation
- [ ] Code examples are taken from actual codebase
- [ ] Configuration section lists all required environment variables
- [ ] Project structure matches actual folder organization
- [ ] No generic or placeholder content remains
- [ ] Documentation has been validated against the running application

## 📚 Reference Materials

### Codebase Locations
- **Main Service**: `services/the0-api/`
- **Package File**: `services/the0-api/package.json`
- **Controllers**: `services/the0-api/src/modules/*/controllers/`
- **Services**: `services/the0-api/src/modules/*/services/`
- **Database**: `services/the0-api/src/db/`
- **Configuration**: `services/the0-api/src/config/`

### Original Story Reference
- **Main Story**: `stories/infrastructure.md` (lines 44-68)

### Documentation File
- **Target File**: `src/docs/platform-development-guide/services/api-server.md`

## 🔗 Related Stories
- CLI Tool documentation fix
- Frontend Dashboard documentation fix
- Runtime Services documentation fix
- Data Architecture documentation fix

---

**Priority**: High
**Estimated Effort**: 4-6 hours
**Dependencies**: Access to API server codebase
