package constants

// Default runtime paths
const (
	// DefaultCodePath is the default directory for bot code
	DefaultCodePath = "/bot"

	// DefaultStatePath is the default directory for bot state
	DefaultStatePath = "/state"

	// DefaultLogsPath is the default directory for bot logs
	DefaultLogsPath = "/var/the0/logs"

	// DefaultDoneFilePath is the path to the done marker file for scheduled bots
	DefaultDoneFilePath = "/var/the0/done"
)

// Language wrapper paths
const (
	// PythonWrapperPath is the path to the Python wrapper script
	PythonWrapperPath = "/app/wrappers/python_bot.py"

	// NodeWrapperPath is the path to the Node.js wrapper script
	NodeWrapperPath = "/app/wrappers/node_bot.js"
)
