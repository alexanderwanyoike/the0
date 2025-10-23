# CLI Tool Documentation Fix Story

## 📋 Objective

Validate and correct the CLI Tool documentation (`src/docs/platform-development-guide/services/cli-tool.md`) to accurately reflect the actual Go implementation of the the0 CLI in the codebase.

## 🎯 Goal

Ensure the CLI Tool documentation accurately describes:
- All available CLI commands and subcommands
- Command-line flags and options
- Configuration file format and .the0ignore patterns
- Docker-based Python vendoring process
- Authentication and API key management
- Auto-update mechanism
- Build and distribution process
- Actual project structure

## 🔍 Validation Checklist

### 1. Technology Stack Verification
- [ ] Verify Go version from go.mod
- [ ] Confirm Cobra framework version
- [ ] Check Docker client library version
- [ ] Verify other dependencies (viper, etc.)
- [ ] Document build requirements

**Location to check**: `services/the0-cli/go.mod`

### 2. Command Structure Validation
- [ ] Document all root-level commands
- [ ] List all subcommands for each command
- [ ] Verify command aliases
- [ ] Document command flags and options
- [ ] Check default values for flags
- [ ] Verify required vs optional flags

**Commands to document**:
- [ ] `the0 login` - Authentication
- [ ] `the0 logout` - Clear authentication
- [ ] `the0 status` - Show login status
- [ ] `the0 deploy` - Deploy custom bot
- [ ] `the0 list` - List bots
- [ ] `the0 update` - Update bot
- [ ] `the0 delete` - Delete bot
- [ ] `the0 backtest deploy` - Deploy backtest
- [ ] `the0 backtest list` - List backtests
- [ ] `the0 backtest delete` - Delete backtest
- [ ] `the0 version` - Show version
- [ ] `the0 upgrade` - Self-update CLI
- [ ] Any other commands found in code

**Locations to check**:
- `services/the0-cli/cmd/`
- `services/the0-cli/cmd/root.go`
- `services/the0-cli/cmd/*/*.go`

### 3. Configuration Management
- [ ] Verify .the0ignore format and patterns
- [ ] Document configuration file structure
- [ ] Check where config files are stored
- [ ] Verify default configuration values
- [ ] Document environment variable overrides
- [ ] Check config validation logic

**Locations to check**:
- `services/the0-cli/internal/config/`
- `services/the0-cli/internal/ignore/`
- Example config files in repo

### 4. Docker Vendoring Process
- [ ] Document Docker image used for vendoring
- [ ] Verify Python version used in vendoring
- [ ] Document pip installation process
- [ ] Check requirements.txt handling
- [ ] Verify file copying and packaging
- [ ] Document error handling for vendoring

**Locations to check**:
- `services/the0-cli/internal/vendor/`
- `services/the0-cli/internal/docker/`

### 5. Authentication & API Integration
- [ ] Document authentication flow
- [ ] Verify API key storage location
- [ ] Check API endpoint configuration
- [ ] Document token refresh mechanism
- [ ] Verify SSL/TLS handling
- [ ] Check error handling for API failures

**Locations to check**:
- `services/the0-cli/internal/auth/`
- `services/the0-cli/internal/api/`
- `services/the0-cli/internal/client/`

### 6. Auto-Update System
- [ ] Document version checking mechanism
- [ ] Verify update server endpoint
- [ ] Check binary download and verification
- [ ] Document rollback mechanism (if any)
- [ ] Verify platform-specific update logic
- [ ] Check update notification system

**Locations to check**:
- `services/the0-cli/internal/update/`
- `services/the0-cli/cmd/upgrade.go`

### 7. Build & Distribution
- [ ] Document build process (Makefile, scripts)
- [ ] Verify cross-compilation targets
- [ ] Check binary naming conventions
- [ ] Document release process
- [ ] Verify distribution channels
- [ ] Check code signing (if implemented)

**Locations to check**:
- `services/the0-cli/Makefile`
- `services/the0-cli/scripts/`
- `.github/workflows/` (if exists)

### 8. Project Structure
- [ ] Verify actual folder structure
- [ ] Document internal packages
- [ ] Check for shared utilities
- [ ] Verify main.go structure
- [ ] Document testing structure

**Locations to check**:
- `services/the0-cli/`

## 🐛 Known Issues to Address

### Issues Identified
1. **Command Documentation**: May not include all actual commands or flags
2. **Generic Examples**: Replace with actual command output
3. **Configuration Format**: May not match actual implementation
4. **Docker Process**: Vendoring details may be incomplete or inaccurate
5. **Error Messages**: Document actual error messages users will see
6. **Platform Differences**: May not document OS-specific behavior

### Sections Needing Verification
- **Command structure table**: Update with all actual commands
- **Configuration examples**: Replace with actual config format
- **Docker vendoring section**: Verify against implementation
- **Authentication flow**: Document actual API interaction
- **Update mechanism**: Verify self-update process
- **Code examples**: Replace with real CLI output

## 📝 Tasks to Complete

### Phase 1: Codebase Analysis
1. Navigate to CLI codebase
2. Review go.mod for dependency versions
3. Examine all cmd/ files to document commands
4. Test each command to capture actual output
5. Review config and ignore file handling
6. Document Docker vendoring implementation
7. Check authentication and API client code

### Phase 2: Documentation Updates
1. Update Technology Stack with accurate Go and dependency versions
2. Create comprehensive command reference table
3. Document all flags and options for each command
4. Update configuration examples with actual formats
5. Rewrite Docker vendoring section with accurate details
6. Document authentication flow and API key management
7. Update auto-update mechanism description
8. Add actual command output examples
9. Document error messages and troubleshooting

### Phase 3: Validation
1. Run each documented command to verify accuracy
2. Test configuration files against actual CLI
3. Verify .the0ignore patterns work as documented
4. Test authentication flow
5. Validate build process
6. Check cross-platform compatibility notes

## ✅ Success Criteria

- [ ] All CLI commands are documented with accurate syntax
- [ ] All flags and options are correctly documented
- [ ] Configuration file format matches actual implementation
- [ ] .the0ignore patterns are accurately described
- [ ] Docker vendoring process is detailed and accurate
- [ ] Authentication flow matches actual implementation
- [ ] Auto-update mechanism is correctly explained
- [ ] Build process is documented
- [ ] Command output examples are from actual CLI
- [ ] Error messages match what users actually see
- [ ] Platform-specific notes are included where relevant

## 📚 Reference Materials

### Codebase Locations
- **Main CLI**: `services/the0-cli/`
- **Go Module**: `services/the0-cli/go.mod`
- **Commands**: `services/the0-cli/cmd/`
- **Internal Packages**: `services/the0-cli/internal/`
- **Build Files**: `services/the0-cli/Makefile`

### Original Story Reference
- **Main Story**: `stories/infrastructure.md` (lines 70-92)

### Documentation File
- **Target File**: `src/docs/platform-development-guide/services/cli-tool.md`

## 🔗 Related Stories
- API Server documentation fix (for API endpoints)
- Custom Bot Development documentation (CLI usage context)
- Docker Deployment documentation

---

**Priority**: High
**Estimated Effort**: 4-6 hours
**Dependencies**: Access to CLI codebase, ability to build and run CLI
