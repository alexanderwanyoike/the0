# User Story: Backtest CLI Support

## 📋 Story Information

**Title**: Add Backtest Management Commands to the0 CLI
**Feature**: Backtest deployment and management
**Priority**: High
**Estimated Effort**: Medium

## 🎯 Purpose

The the0 CLI currently allows users to deploy and manage trading bot instances but lacks the ability to deploy and manage backtests for custom bots. This feature will enable users to:

1. **Deploy backtests** using custom bot configurations
2. **List and monitor** backtest execution status
3. **Delete completed or unwanted backtests**
4. **Access backtest logs** for debugging and analysis

This feature completes the bot development lifecycle by allowing users to test their custom strategies before deploying them as live trading bots.

## 👥 User Scenarios

### Scenario 1: Deploying a New Backtest
**As a** trading bot developer
**When I** have created a custom bot and want to test its performance against historical data
**Then I** can deploy a backtest using a simple configuration file and get feedback on its execution

### Scenario 2: Monitoring Backtest Status
**As a** trading bot developer
**When I** have deployed one or more backtests
**Then I** can list all my backtests with their current status, execution details, and results

### Scenario 3: Managing Backtest Lifecycle
**As a** trading bot developer
**When I** have completed or unwanted backtests
**Then I** can safely delete them with proper confirmation

### Scenario 4: Debugging Backtest Issues
**As a** trading bot developer
**When I** need to troubleshoot backtest execution problems
**Then I** can access detailed logs to identify and fix issues

## ✅ Acceptance Criteria

### Functional Requirements

1. **Deploy Command (`the0 backtest deploy <config.json>`)**
   - ✅ Accepts a flat JSON configuration file (same format as bot deploy)
   - ✅ Validates required fields: `name`, `type`, `version`
   - ✅ Transforms flat config to API format internally
   - ✅ Provides clear deployment status and progress feedback
   - ✅ Returns backtest ID upon successful deployment
   - ✅ Handles authentication errors with retry mechanism

2. **List Command (`the0 backtest list`)**
   - ✅ Displays all backtests for the authenticated user
   - ✅ Shows backtest ID, name, status, creation date, and key metrics
   - ✅ Uses table format consistent with `the0 bot list`
   - ✅ Handles empty results with appropriate messaging
   - ✅ Includes status indicators (pending, running, completed, failed)

3. **Delete Command (`the0 backtest delete <id>`)**
   - ✅ Accepts backtest ID as parameter
   - ✅ Shows confirmation prompt before deletion
   - ✅ Prevents accidental deletion with safety checks
   - ✅ Provides clear success/error feedback
   - ✅ Handles non-existent backtest IDs gracefully

### Technical Requirements

4. **API Integration**
   - ✅ Integrates with existing backtest API endpoints
   - ✅ Uses same authentication patterns as bot commands
   - ✅ Implements retry logic for network failures
   - ✅ Handles API errors with user-friendly messages

5. **Configuration Management**
   - ✅ Supports flat JSON configuration structure
   - ✅ Validates configuration format and required fields
   - ✅ Provides clear error messages for validation failures
   - ✅ Transforms between CLI and API formats seamlessly

6. **Error Handling**
   - ✅ Consistent error handling with existing CLI commands
   - ✅ Graceful fallback for authentication issues
   - ✅ Clear error messages with suggested actions
   - ✅ Proper exit codes for scripting compatibility

7. **User Experience**
   - ✅ Consistent CLI styling and output format with bot commands
   - ✅ Rich status indicators and progress feedback
   - ✅ Help documentation and usage examples
   - ✅ Emoji usage consistent with existing commands

## 🔧 Technical Implementation

### Required Commands

```bash
# Deploy a new backtest
the0 backtest deploy <config.json>

# List all backtests
the0 backtest list

# Delete a backtest
the0 backtest delete <backtest_id>
```

### Configuration Format

**Flat JSON Structure** (same as bot deploy):
```json
{
  "name": "market-maker-backtest",
  "type": "market-maker/my-trading-bot",
  "version": "1.0.0",
  "symbol": "BTCUSDT",
  "timeframe": "1h",
  "strategy_params": {
    "risk_level": "medium",
    "max_position_size": 1000,
    "spread_threshold": 0.001
  },
  "backtest_params": {
    "start_date": "2024-01-01",
    "end_date": "2024-12-31",
    "initial_capital": 10000,
    "commission": 0.001
  }
}
```

**Required Fields:**
- `name` - Backtest name (string)
- `type` - Bot type in format "category/name" (string)
- `version` - Bot version (string)

**Optional Fields:**
- Any strategy-specific parameters
- Backtest execution parameters
- Market data configuration

### API Integration Details

**Endpoints to be used:**
- `POST /backtest` - Create new backtest
- `GET /backtest` - List all backtests
- `DELETE /backtest/:id` - Delete specific backtest
- `GET /backtest/:id/logs` - Get backtest logs (future feature)

**Request Transformation:**
```go
// CLI receives flat config, transforms to API format
APIRequest := {
    "name": backtestName,
    "config": {
        "type": config["type"],
        "version": config["version"],
        // ... other parameters passed through
    }
}
```

### File Structure Changes

**New Files:**
- `cmd/backtest.go` - Backtest command implementation
- `tests/backtest_test.go` - Comprehensive test suite
- `stories/backtest-cli-support.md` - This story document

**Modified Files:**
- `main.go` - Add backtest command to root command group
- `internal/api.go` - Add backtest API methods and data structures

### Data Structures

**New Structs to add:**
```go
type Backtest struct {
    ID        string         `json:"id"`
    Name      string         `json:"name"`
    Config    map[string]any `json:"config"`
    Analysis  any            `json:"analysis"`
    Status    string         `json:"status"`
    CreatedAt string         `json:"createdAt"`
    UpdatedAt string         `json:"updatedAt"`
    UserID    string         `json:"userId"`
    CustomBotID string       `json:"customBotId"`
}

type BacktestDeployRequest struct {
    Name   string         `json:"name"`
    Config map[string]any `json:"config"`
}
```

## 📊 Success Metrics

1. **Feature Completeness**: All three commands work correctly
2. **User Experience**: Consistent with existing bot command patterns
3. **Error Handling**: Graceful failure handling with clear messages
4. **Test Coverage**: 90%+ code coverage for new functionality
5. **Documentation**: Complete with examples and usage patterns

## 🚀 Implementation Timeline

**Phase 1: Foundation (Day 1)**
- Create backtest command structure
- Implement API client methods
- Add basic data structures

**Phase 2: Core Features (Day 2)**
- Implement deploy command with validation
- Implement list command with table display
- Implement delete command with safety checks

**Phase 3: Polish & Testing (Day 3)**
- Add comprehensive error handling
- Implement retry logic and authentication
- Create test suite and documentation

## 🔗 Dependencies

### Existing Dependencies
- Authentication system (`internal/auth.go`)
- API client patterns (`internal/api.go`)
- CLI framework (Cobra)
- Table formatting and styling libraries

### API Dependencies
- Backtest controller endpoints must be available
- Authentication middleware must be functional
- Backtest service must handle requests correctly

## 🚨 Risks and Mitigations

**Risk**: API endpoints not ready or have different response format
**Mitigation**: Work with API team to align on interface, implement flexible response handling

**Risk**: Backtest execution takes long time, users need progress feedback
**Mitigation**: Provide clear status indicators in list command, consider future progress monitoring

**Risk**: Configuration validation complexity
**Mitigation**: Start with simple validation, enhance based on user feedback

## 📝 Definition of Done

A backtest CLI feature is considered complete when:

1. ✅ All three commands (deploy, list, delete) are implemented
2. ✅ Configuration validation works correctly
3. ✅ Error handling is comprehensive and user-friendly
4. ✅ Test coverage meets quality standards (>90%)
5. ✅ Documentation is updated with examples
6. ✅ CLI styling is consistent with existing commands
7. ✅ Authentication integration works seamlessly
8. ✅ Feature has been tested against real API endpoints
9. ✅ Code review has been completed and approved
10. ✅ Feature works in both development and production environments

## 🎁 Future Enhancements

**Phase 2 Features (not in scope for initial release):**
- `the0 backtest logs <id>` - View backtest execution logs
- `the0 backtest status <id>` - Get detailed status and progress
- `the0 backtest results <id>` - Display backtest results and analysis
- Real-time progress monitoring during deployment
- Backtest comparison tools

**Phase 3 Features:**
- Batch backtest operations
- Backtest configuration templates
- Integration with bot development workflow
- Performance analytics and reporting