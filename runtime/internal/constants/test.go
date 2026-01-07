package constants

import "time"

// Test timeouts and intervals
const (
	// TestInvalidPort is a port number guaranteed to be invalid
	TestInvalidPort = 99999

	// Test sync intervals
	TestDefaultSyncInterval = 60 * time.Second
	TestCustomSyncInterval  = 30 * time.Second

	// Test wait timeouts
	TestShortWait  = 200 * time.Millisecond
	TestMediumWait = 600 * time.Millisecond
	TestLongWait   = 3 * time.Second
)

// Test identifiers
const (
	// TestBotID is a standard bot ID for tests
	TestBotID = "test-bot"

	// TestCodeFile is a sample code file path
	TestCodeFile = "custom-bot/v2.0.0/code.zip"

	// TestDoneFilePath is a path for done file testing
	TestDoneFilePath = "/tmp/done"
)

// Test paths - these match production defaults
const (
	// TestBotDir is the standard bot code directory
	TestBotDir = "/bot"

	// TestStateDir is the standard state directory
	TestStateDir = "/state"

	// TestLogsDir is the standard logs directory
	TestLogsDir = "/var/the0/logs"
)

// Test wrapper paths - these match production wrapper locations
const (
	// TestPythonWrapperPath is the path to the Python wrapper
	TestPythonWrapperPath = "/app/wrappers/python_bot.py"

	// TestNodeWrapperPath is the path to the Node.js wrapper
	TestNodeWrapperPath = "/app/wrappers/node_bot.js"
)
