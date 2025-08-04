name: "Base PRP Template v2 - Context-Rich with Validation Loops"
description: |

## Purpose
Template optimized for AI agents to implement features with sufficient context and self-validation capabilities to achieve working code through iterative refinement.

## Core Principles
1. **Context is King**: Include ALL necessary documentation, examples, and caveats
2. **Validation Loops**: Provide executable tests/lints the AI can run and fix
3. **Information Dense**: Use keywords and patterns from the codebase
4. **Progressive Success**: Start simple, validate, then enhance
5. **Global rules**: Be sure to follow all rules in CLAUDE.md

---

## Goal
[What needs to be built - be specific about the end state and desires]

## Why
- [Business value and user impact]
- [Integration with existing features]
- [Problems this solves and for whom]

## What
[User-visible behavior and technical requirements]

### Success Criteria
- [ ] [Specific measurable outcomes]

## All Needed Context

### Documentation & References (list all context needed to implement the feature)
```yaml
# MUST READ - Include these in your context window
- url: https://cobra.dev/
  why: CLI framework patterns, command structure, and flag handling
  
- url: https://pkg.go.dev/github.com/spf13/cobra
  why: Command creation, persistent flags, and command hierarchy
  
- url: https://pkg.go.dev/github.com/spf13/viper
  why: Configuration management and environment variable handling
  
- url: https://pkg.go.dev/github.com/spf13/pflag
  why: Flag parsing and validation patterns

- file: cmd/root.go
  why: Root command setup, global flags, and initialization patterns
  
- file: cmd/version.go
  why: Simple command implementation and output formatting
  
- file: internal/config/config.go
  why: Configuration structure and validation patterns
  
- file: internal/logger/logger.go
  why: Logging setup and structured logging patterns
  
- file: main.go
  why: Application entry point and dependency injection

- doc: https://golang.org/doc/effective_go.html
  section: Error handling
  critical: Use error wrapping with fmt.Errorf and proper error messages

- doc: https://pkg.go.dev/context
  section: Context usage
  critical: Use context.Context for cancellation and timeout handling

- docfile: PRPs/ai_docs/go-patterns.md
  why: Project-specific patterns and coding standards

- docfile: PRPs/ai_docs/cli-conventions.md
  why: CLI design patterns and user experience guidelines

- docfile: CLAUDE.md
  why: Global coding rules and project-specific requirements
```

### Current Codebase tree (run `tree` in the root of the project) to get an overview of the codebase
```bash

```

### Desired Codebase tree with files to be added and responsibility of file
```bash

```

### Known Gotchas of our codebase & Library Quirks
```go
// CRITICAL: [Library name] requires [specific setup]
// Example: Cobra requires PersistentPreRun for global flag validation
// Example: Viper key names must match struct field names or use mapstructure tags
// Example: pflag requires explicit binding to viper with viper.BindPFlag
```

## Implementation Blueprint

### Data models and structure
Create the core data models and types to ensure type safety and consistency.
```go
Examples: 
 - Struct definitions with json/yaml tags
 - Interface definitions for abstraction
 - Custom error types
 - Enum-like constants with iota
 - Configuration structs with validation

```

### list of tasks to be completed to fullfill the PRP in the order they should be completed

```yaml
Task 1: Create Core Data Models
CREATE internal/models/feature.go:
  - MIRROR pattern from: internal/models/existing.go
  - ADD struct with proper json/yaml tags
  - IMPLEMENT validation methods
  - PRESERVE existing naming conventions

CREATE internal/types/feature.go:
  - DEFINE interfaces for feature operations
  - ADD custom error types following existing patterns
  - KEEP consistent with existing type definitions

Task 2: Implement Core Service
CREATE internal/service/feature.go:
  - MIRROR pattern from: internal/service/existing.go
  - IMPLEMENT interface from types package
  - PRESERVE existing error handling patterns
  - KEEP dependency injection patterns consistent

Task 3: Create Command Layer
CREATE cmd/feature.go:
  - MIRROR pattern from: cmd/existing.go
  - ADD cobra.Command with proper flags
  - PRESERVE existing flag naming conventions
  - KEEP consistent help text formatting

Task 4: Add Configuration Support
MODIFY internal/config/config.go:
  - FIND existing config struct
  - ADD new feature-related fields
  - PRESERVE existing validation patterns
  - KEEP consistent with existing field naming

Task 5: Update Root Command
MODIFY cmd/root.go:
  - FIND command registration section
  - ADD new feature command to root
  - PRESERVE existing command ordering
  - KEEP consistent initialization patterns

...
```

### Per task pseudocode as needed added to each task
```go

// Task 1
// Pseudocode with CRITICAL details dont write entire code
type FeatureService struct {
    config *config.Config
    logger *slog.Logger
    client *http.Client
}

func NewFeatureService(cfg *config.Config, logger *slog.Logger) *FeatureService {
    // PATTERN: Always validate config in constructor
    if cfg == nil {
        panic("config cannot be nil") // CRITICAL: Fail fast on nil dependencies
    }
    
    return &FeatureService{
        config: cfg,
        logger: logger,
        client: &http.Client{
            Timeout: time.Duration(cfg.HTTPTimeout) * time.Second,
        },
    }
}

func (s *FeatureService) CreateFeature(ctx context.Context, req *CreateFeatureRequest) (*Feature, error) {
    // PATTERN: Always validate input
    if err := req.Validate(); err != nil {
        return nil, fmt.Errorf("invalid request: %w", err)
    }
    
    // GOTCHA: HTTP client needs context for cancellation
    httpReq, err := http.NewRequestWithContext(ctx, "POST", s.config.APIEndpoint, body)
    if err != nil {
        return nil, fmt.Errorf("failed to create request: %w", err)
    }
    
    // PATTERN: Always log operations with structured logging
    s.logger.Info("creating feature", 
        slog.String("name", req.Name),
        slog.String("operation", "create_feature"))
    
    resp, err := s.client.Do(httpReq)
    if err != nil {
        // CRITICAL: Wrap errors with context
        return nil, fmt.Errorf("API request failed: %w", err)
    }
    defer resp.Body.Close()
    
    // PATTERN: Handle HTTP status codes explicitly
    if resp.StatusCode >= 400 {
        return nil, fmt.Errorf("API returned status %d", resp.StatusCode)
    }
    
    var feature Feature
    if err := json.NewDecoder(resp.Body).Decode(&feature); err != nil {
        return nil, fmt.Errorf("failed to decode response: %w", err)
    }
    
    return &feature, nil
}

// Command pattern
var featureCmd = &cobra.Command{
    Use:   "feature",
    Short: "Manage features",
    Long:  `Create, update, and manage application features.`,
    RunE: func(cmd *cobra.Command, args []string) error {
        // PATTERN: Use context from command
        ctx := cmd.Context()
        
        // GOTCHA: Viper requires explicit binding
        viper.BindPFlag("feature.name", cmd.Flags().Lookup("name"))
        
        name := viper.GetString("feature.name")
        if name == "" {
            return fmt.Errorf("feature name is required")
        }
        
        // PATTERN: Get dependencies from context or globals
        service := getFeatureService(ctx)
        
        feature, err := service.CreateFeature(ctx, &CreateFeatureRequest{
            Name: name,
        })
        if err != nil {
            return fmt.Errorf("failed to create feature: %w", err)
        }
        
        // PATTERN: Use consistent output formatting
        fmt.Printf("Feature created: %s (ID: %s)\n", feature.Name, feature.ID)
        return nil
    },
}

func init() {
    // CRITICAL: Add flags before registering command
    featureCmd.Flags().StringP("name", "n", "", "Feature name (required)")
    featureCmd.MarkFlagRequired("name")
    
    // PATTERN: Add to root command
    rootCmd.AddCommand(featureCmd)
}
```


## Validation Loop

### Level 1: Syntax & Style
```bash
# Run these FIRST - fix any errors before proceeding
go fmt ./...                         # Format code
go vet ./...                        # Static analysis
golangci-lint run                   # Comprehensive linting
go mod tidy                         # Clean up dependencies

# Expected: No errors. If errors, READ the error and fix.
```

### Level 2: Unit Tests
```go
// feature_test.go - Use table-driven tests following Go conventions
func TestFeatureService_CreateFeature(t *testing.T) {
    tests := []struct {
        name        string
        request     *CreateFeatureRequest
        mockSetup   func(*httptest.Server)
        want        *Feature
        wantErr     bool
        wantErrType error
    }{
        {
            name: "successful creation",
            request: &CreateFeatureRequest{
                Name: "test-feature",
                Description: "Test feature",
            },
            mockSetup: func(server *httptest.Server) {
                // Setup mock HTTP server response
            },
            want: &Feature{
                ID:   "test-id",
                Name: "test-feature",
            },
            wantErr: false,
        },
        {
            name: "invalid request",
            request: &CreateFeatureRequest{
                Name: "", // Invalid: empty name
            },
            wantErr:     true,
            wantErrType: &ValidationError{},
        },
        {
            name: "API timeout",
            request: &CreateFeatureRequest{
                Name: "test-feature",
            },
            mockSetup: func(server *httptest.Server) {
                // Setup server to simulate timeout
                time.Sleep(2 * time.Second)
            },
            wantErr:     true,
            wantErrType: &APIError{},
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            // Setup
            server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
                if tt.mockSetup != nil {
                    tt.mockSetup(server)
                }
            }))
            defer server.Close()

            cfg := &config.Config{
                APIEndpoint: server.URL,
                HTTPTimeout: 1,
            }
            
            service := NewFeatureService(cfg, slog.Default())
            
            // Execute
            ctx := context.Background()
            got, err := service.CreateFeature(ctx, tt.request)
            
            // Assert
            if tt.wantErr {
                assert.Error(t, err)
                if tt.wantErrType != nil {
                    assert.IsType(t, tt.wantErrType, err)
                }
                return
            }
            
            assert.NoError(t, err)
            assert.Equal(t, tt.want, got)
        })
    }
}

func TestFeatureCommand(t *testing.T) {
    tests := []struct {
        name    string
        args    []string
        wantErr bool
        wantOut string
    }{
        {
            name: "create feature success",
            args: []string{"feature", "--name", "test-feature"},
            wantErr: false,
            wantOut: "Feature created: test-feature",
        },
        {
            name: "missing required flag",
            args: []string{"feature"},
            wantErr: true,
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            // Setup command with test dependencies
            cmd := &cobra.Command{}
            cmd.SetArgs(tt.args)
            
            // Capture output
            buf := &bytes.Buffer{}
            cmd.SetOut(buf)
            cmd.SetErr(buf)
            
            // Execute
            err := cmd.Execute()
            
            // Assert
            if tt.wantErr {
                assert.Error(t, err)
                return
            }
            
            assert.NoError(t, err)
            assert.Contains(t, buf.String(), tt.wantOut)
        })
    }
}
```

```bash
# Run and iterate until passing:
go test -v ./...
go test -race ./...                 # Check for race conditions
go test -cover ./...                # Check test coverage

# If failing: Read error, understand root cause, fix code, re-run
```

### Level 3: Integration Test
Ensure end-to-end functionality works correctly.
```go
// integration_test.go
func TestFeatureCommandIntegration(t *testing.T) {
    if testing.Short() {
        t.Skip("skipping integration test")
    }

    // Setup test environment
    testDir := t.TempDir()
    configFile := filepath.Join(testDir, "config.yaml")
    
    // Create test config
    cfg := &config.Config{
        APIEndpoint: "http://localhost:8080", // Assume test server
        HTTPTimeout: 30,
    }
    
    configData, err := yaml.Marshal(cfg)
    require.NoError(t, err)
    
    err = os.WriteFile(configFile, configData, 0644)
    require.NoError(t, err)

    tests := []struct {
        name    string
        args    []string
        wantErr bool
    }{
        {
            name: "create feature end-to-end",
            args: []string{
                "feature",
                "--name", "integration-test-feature",
                "--config", configFile,
            },
            wantErr: false,
        },
        {
            name: "list features",
            args: []string{
                "feature",
                "list",
                "--config", configFile,
            },
            wantErr: false,
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            // Build the CLI binary
            cmd := exec.Command("go", "build", "-o", filepath.Join(testDir, "testcli"), "./cmd/main.go")
            err := cmd.Run()
            require.NoError(t, err)

            // Run the CLI command
            cliCmd := exec.Command(filepath.Join(testDir, "testcli"), tt.args...)
            cliCmd.Dir = testDir
            
            output, err := cliCmd.CombinedOutput()
            
            if tt.wantErr {
                assert.Error(t, err)
                return
            }
            
            assert.NoError(t, err, "CLI output: %s", string(output))
            t.Logf("CLI output: %s", string(output))
        })
    }
}
```

## Final validation Checklist
- [ ] All tests pass: `make test`
- [ ] No linting errors: `make lint`
- [ ] No race conditions: `go test -race ./...`
- [ ] Build succeeds: `make build`
- [ ] CLI help text is clear: `./app help`
- [ ] Configuration validation works
- [ ] Error messages are user-friendly

---

## Anti-Patterns to Avoid
- ❌ Don't ignore errors - always handle them explicitly
- ❌ Don't use global variables for state
- ❌ Don't skip context propagation in long-running operations
- ❌ Don't hardcode configuration values
- ❌ Don't use panic for regular error cases
- ❌ Don't ignore golangci-lint warnings

## PR Message

When done write a PR message summarizing the feature the changes made, the validation steps taken, and any known issues or follow-up tasks. Use the following template:

```markdown
## Feature: [Feature Name]
[Brief description of the feature and its purpose]

## Background
[Context on why this feature is needed and how it fits into the overall CLI]

## Changes Made
[List of major changes made in the codebase]

## Validation Steps
[List of steps taken to validate the feature]

## Known Issues
[List any known issues or limitations of the implementation]
```

Save as: `PR_MSG/{feature-name}.md`