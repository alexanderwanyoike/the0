package cmd

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"strings"

	"github.com/fatih/color"
	"github.com/olekukonko/tablewriter"
	"github.com/spf13/cobra"

	"the0/internal/local"
	"the0/internal/logger"
)

// NewLocalCmd creates the `the0 local` command group
func NewLocalCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "local",
		Short: "Manage the local development environment",
		Long: `Manage the local the0 development environment.

Start, stop, and monitor the full Docker Compose stack from anywhere,
without needing to cd into the docker/ directory.

Quick start:
  the0 local init --repo-path /path/to/the0
  the0 local start
  the0 local status`,
	}

	cmd.AddCommand(
		newLocalInitCmd(),
		newLocalStartCmd(),
		newLocalStopCmd(),
		newLocalRestartCmd(),
		newLocalStatusCmd(),
		newLocalLogsCmd(),
		newLocalDevCmd(),
		newLocalUninstallCmd(),
	)

	return cmd
}

// --- init ---

func newLocalInitCmd() *cobra.Command {
	var repoPath string
	var prebuilt bool

	cmd := &cobra.Command{
		Use:   "init",
		Short: "Initialize the local environment",
		Long: `Initialize the local environment by detecting Docker prerequisites,
validating the repository path, and extracting compose files to ~/.the0/compose/.

This must be run before 'the0 local start'.`,
		RunE: func(cmd *cobra.Command, args []string) error {
			// If no repo path given, try current directory
			if repoPath == "" {
				cwd, err := os.Getwd()
				if err != nil {
					return fmt.Errorf("failed to get current directory: %w", err)
				}
				repoPath = cwd
			}

			// Check Docker prerequisites
			logger.StartSpinner("Checking Docker prerequisites")
			if err := local.CheckAllPrerequisites(); err != nil {
				logger.StopSpinnerWithError("Docker check failed")
				return err
			}
			logger.StopSpinnerWithSuccess("Docker is ready")

			// Handle --prebuilt
			if prebuilt {
				logger.Warning("Prebuilt images are not yet available. Using local build mode.")
				logger.Info("Once published images exist, --prebuilt will skip building from source.")
				prebuilt = false
			}

			// Validate repo path
			logger.StartSpinner("Validating repository path")
			if err := local.ValidateRepoPath(repoPath); err != nil {
				logger.StopSpinnerWithError("Invalid repository path")
				return err
			}
			logger.StopSpinner()
			logger.Success("Repository validated: %s", repoPath)

			// Extract compose files
			logger.StartSpinner("Setting up compose files")
			if err := local.ExtractComposeFiles(repoPath, prebuilt); err != nil {
				logger.StopSpinnerWithError("Failed to extract compose files")
				return err
			}

			dir, _ := local.ComposeDir()
			logger.StopSpinner()
			logger.Success("Compose files written to %s", dir)

			// Generate .env
			if err := local.GenerateEnvFile(dir); err != nil {
				logger.Warning("Failed to generate .env file: %v", err)
			}

			logger.Newline()
			logger.Success("Local environment initialized!")
			logger.Info("Run 'the0 local start' to start all services")

			return nil
		},
	}

	cmd.Flags().StringVar(&repoPath, "repo-path", "", "Path to the the0 repository (defaults to current directory)")
	cmd.Flags().BoolVar(&prebuilt, "prebuilt", false, "Use prebuilt Docker images (not yet available)")

	return cmd
}

// --- start ---

func newLocalStartCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "start",
		Short: "Start all local services",
		Long:  `Start the full the0 stack using Docker Compose. Builds images from source if needed.`,
		RunE: func(cmd *cobra.Command, args []string) error {
			runner, err := local.NewComposeRunner()
			if err != nil {
				return err
			}

			// Build compose services
			logger.StartSpinner("Building services")
			if err := runner.Build(); err != nil {
				logger.StopSpinnerWithError("Build failed")
				return fmt.Errorf("docker compose build failed: %w", err)
			}
			logger.StopSpinnerWithSuccess("Services built")

			// Start services
			logger.StartSpinner("Starting services")
			if err := runner.Up(); err != nil {
				logger.StopSpinnerWithError("Failed to start services")
				return fmt.Errorf("docker compose up failed: %w", err)
			}
			logger.StopSpinnerWithSuccess("Services started")

			local.PrintServiceURLs()
			logger.Info("Use 'the0 local status' to check container health")
			logger.Info("Use 'the0 local logs [service]' to view logs")

			return nil
		},
	}
}

// --- stop ---

func newLocalStopCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "stop",
		Short: "Stop all local services",
		Long:  `Stop and remove all running the0 containers. Data volumes are preserved.`,
		RunE: func(cmd *cobra.Command, args []string) error {
			runner, err := local.NewComposeRunner()
			if err != nil {
				return err
			}

			logger.StartSpinner("Stopping services")
			if err := runner.Down(); err != nil {
				logger.StopSpinnerWithError("Failed to stop services")
				return fmt.Errorf("docker compose down failed: %w", err)
			}
			logger.StopSpinnerWithSuccess("All services stopped")

			return nil
		},
	}
}

// --- restart ---

func newLocalRestartCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "restart",
		Short: "Restart all local services (rebuilds images)",
		Long:  `Stop all services, rebuild images from source, and start again.`,
		RunE: func(cmd *cobra.Command, args []string) error {
			runner, err := local.NewComposeRunner()
			if err != nil {
				return err
			}

			// Stop
			logger.StartSpinner("Stopping services")
			if err := runner.Down(); err != nil {
				logger.StopSpinnerWithError("Failed to stop services")
				return fmt.Errorf("docker compose down failed: %w", err)
			}
			logger.StopSpinnerWithSuccess("Services stopped")

			// Rebuild
			logger.StartSpinner("Rebuilding services")
			if err := runner.Build(); err != nil {
				logger.StopSpinnerWithError("Build failed")
				return fmt.Errorf("docker compose build failed: %w", err)
			}
			logger.StopSpinnerWithSuccess("Services rebuilt")

			// Start
			logger.StartSpinner("Starting services")
			if err := runner.Up(); err != nil {
				logger.StopSpinnerWithError("Failed to start services")
				return fmt.Errorf("docker compose up failed: %w", err)
			}
			logger.StopSpinnerWithSuccess("Services restarted")

			local.PrintServiceURLs()

			return nil
		},
	}
}

// --- status ---

// composePS represents a single container from `docker compose ps --format json`
type composePS struct {
	Name    string `json:"Name"`
	Service string `json:"Service"`
	State   string `json:"State"`
	Health  string `json:"Health"`
	Status  string `json:"Status"`
	Ports   string `json:"Ports"`
}

func newLocalStatusCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "status",
		Short: "Show status of all local services",
		Long:  `Display a table of all running containers with their health status and ports.`,
		RunE: func(cmd *cobra.Command, args []string) error {
			runner, err := local.NewComposeRunner()
			if err != nil {
				return err
			}

			output, err := runner.PS()
			if err != nil {
				return fmt.Errorf("failed to get container status: %w", err)
			}

			if strings.TrimSpace(output) == "" {
				logger.Info("No containers running. Run 'the0 local start' first.")
				return nil
			}

			// Parse JSON lines (docker compose ps --format json outputs one JSON per line)
			var containers []composePS
			for _, line := range strings.Split(strings.TrimSpace(output), "\n") {
				line = strings.TrimSpace(line)
				if line == "" {
					continue
				}
				var c composePS
				if err := json.Unmarshal([]byte(line), &c); err != nil {
					logger.Verbose("Failed to parse line: %s", line)
					continue
				}
				containers = append(containers, c)
			}

			if len(containers) == 0 {
				logger.Info("No containers running. Run 'the0 local start' first.")
				return nil
			}

			// Build table
			table := tablewriter.NewWriter(os.Stdout)
			table.Header("Service", "Name", "State", "Health", "Status")

			green := color.New(color.FgGreen).SprintFunc()
			red := color.New(color.FgRed).SprintFunc()
			yellow := color.New(color.FgYellow).SprintFunc()

			for _, c := range containers {
				// Resolve friendly name
				friendlyName := c.Service
				for name, svc := range local.ServiceRegistry {
					if svc.ComposeService == c.Service {
						friendlyName = name
						break
					}
				}

				// Colorize health
				health := c.Health
				switch strings.ToLower(health) {
				case "healthy":
					health = green(health)
				case "unhealthy":
					health = red(health)
				case "starting":
					health = yellow(health)
				default:
					if health == "" {
						health = "-"
					}
				}

				// Colorize state
				state := c.State
				switch strings.ToLower(state) {
				case "running":
					state = green(state)
				case "exited", "dead":
					state = red(state)
				case "restarting":
					state = yellow(state)
				}

				table.Append(friendlyName, c.Name, state, health, c.Status)
			}

			logger.Newline()
			if err := table.Render(); err != nil {
				return fmt.Errorf("failed to render table: %w", err)
			}

			return nil
		},
	}
}

// --- logs ---

func newLocalLogsCmd() *cobra.Command {
	var follow bool
	var tail string

	cmd := &cobra.Command{
		Use:   "logs [service]",
		Short: "View logs from local services",
		Long: `View logs from one or all running services.

Examples:
  the0 local logs          # All service logs
  the0 local logs api      # API service logs
  the0 local logs runner   # Bot runner logs
  the0 local logs -f api   # Follow API logs`,
		RunE: func(cmd *cobra.Command, args []string) error {
			runner, err := local.NewComposeRunner()
			if err != nil {
				return err
			}

			service := ""
			if len(args) > 0 {
				resolved, err := local.ResolveFriendlyName(args[0])
				if err != nil {
					return err
				}
				service = resolved
			}

			return runner.Logs(service, follow, tail)
		},
	}

	cmd.Flags().BoolVarP(&follow, "follow", "f", false, "Follow log output")
	cmd.Flags().StringVar(&tail, "tail", "", "Number of lines to show from the end (e.g. '100')")

	return cmd
}

// --- dev ---

func newLocalDevCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "dev",
		Short: "Start in development mode with hot reload",
		Long: `Start the the0 stack with hot-reload enabled for the frontend and API.

Source code changes in frontend/ and api/ are reflected immediately.
Infrastructure services (postgres, mongo, nats, minio) and runtime services
are started normally.`,
		RunE: func(cmd *cobra.Command, args []string) error {
			runner, err := local.NewComposeRunner()
			if err != nil {
				return err
			}

			// Verify repo path still exists (needed for volume mounts)
			if err := local.ValidateRepoPath(runner.State.RepoPath); err != nil {
				return fmt.Errorf("repository path no longer valid: %w\nRun 'the0 local init' again", err)
			}

			// Start infrastructure first and wait for health checks
			logger.StartSpinner("Starting infrastructure services")
			if err := runner.Run("up", "-d", "--wait", "postgres", "mongo", "nats", "minio"); err != nil {
				logger.StopSpinnerWithError("Failed to start infrastructure")
				return err
			}
			logger.StopSpinnerWithSuccess("Infrastructure services ready")

			// Start runtime services (these build from source)
			logger.StartSpinner("Building and starting runtime services")
			if err := runner.Run("up", "-d", "--build", "bot-runner", "bot-scheduler", "query-server"); err != nil {
				logger.StopSpinnerWithError("Failed to start runtime services")
				return err
			}
			logger.StopSpinnerWithSuccess("Runtime services started")

			// Start dev overlay (frontend + API with hot reload)
			logger.StartSpinner("Starting frontend and API in dev mode")
			if err := runner.UpDev(); err != nil {
				logger.StopSpinnerWithError("Failed to start dev services")
				return err
			}
			logger.StopSpinnerWithSuccess("Dev environment ready")

			local.PrintServiceURLs()
			logger.Info("Hot reload is active for frontend and API")
			logger.Info("Use 'the0 local logs api' or 'the0 local logs frontend' to watch")

			return nil
		},
	}
}

// --- uninstall ---

func newLocalUninstallCmd() *cobra.Command {
	var yes bool

	cmd := &cobra.Command{
		Use:   "uninstall",
		Short: "Remove the local environment completely",
		Long: `Stop all containers, remove volumes, and delete the compose configuration.

This removes:
- All running the0 containers
- All Docker volumes (database data, uploads, etc.)
- The ~/.the0/compose/ directory`,
		RunE: func(cmd *cobra.Command, args []string) error {
			if !yes {
				logger.Warning("This will remove ALL local the0 data including databases and uploads.")
				logger.Printf("Type 'yes' to confirm: ")

				reader := bufio.NewReader(os.Stdin)
				input, err := reader.ReadString('\n')
				if err != nil {
					return fmt.Errorf("failed to read input: %w", err)
				}
				if strings.TrimSpace(input) != "yes" {
					logger.Info("Cancelled")
					return nil
				}
			}

			// Try to stop containers and remove volumes
			runner, runnerErr := local.NewComposeRunner()
			if runnerErr == nil {
				logger.StartSpinner("Stopping containers and removing volumes")
				if err := runner.DownWithVolumes(); err != nil {
					logger.StopSpinnerWithError("Failed to stop containers (may already be stopped)")
					logger.Verbose("Error: %v", err)
				} else {
					logger.StopSpinnerWithSuccess("Containers and volumes removed")
				}
			}

			// Remove compose directory
			logger.StartSpinner("Removing compose configuration")
			if err := local.CleanupComposeDir(); err != nil {
				logger.StopSpinnerWithError("Failed to remove compose directory")
				return err
			}
			logger.StopSpinnerWithSuccess("Compose configuration removed")

			logger.Newline()
			logger.Success("Local environment uninstalled")

			return nil
		},
	}

	cmd.Flags().BoolVarP(&yes, "yes", "y", false, "Skip confirmation prompt")

	return cmd
}
