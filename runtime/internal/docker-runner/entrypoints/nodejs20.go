package entrypoints

const NodeJsBotEntrypoint = `#!/usr/bin/env node
/**
 * Node.js Bot Entrypoint Script
 * Handles bot execution with signal management and graceful shutdown.
 */

const fs = require('fs');
const path = require('path');

const controller = new AbortController();

function getResultFilePath() {
    const codeMountDir = process.env.CODE_MOUNT_DIR || 'bot';
    return '/' + codeMountDir + '/result.json';
}

function writeResult(data) {
    const resultPath = getResultFilePath();
    try {
        fs.writeFileSync(resultPath, JSON.stringify(data));
    } catch (error) {
        console.error('RESULT_ERROR: Failed to write result file: ' + error.message);
    }
}

function handleSignal(signal) {
    console.error('SIGNAL: Received ' + signal + ', triggering abort.');
    // --- Trigger the abort signal ---
    controller.abort();
}

// Register signal handlers
process.on('SIGTERM', () => handleSignal('SIGTERM'));
process.on('SIGINT', () => handleSignal('SIGINT'));

console.error('STARTUP: Node.js bot wrapper starting');

function setupEnvironment() {
    // Get entrypoint type from environment
    const entrypointType = process.env.ENTRYPOINT_TYPE || 'bot';
    const workDir = '/' + entrypointType;

    // Change to work directory
    try {
        process.chdir(workDir);
    } catch (error) {
        console.error('CHDIR_ERROR: Could not change to ' + workDir + ' directory: ' + error.message);
        process.exit(1);
    }
}

function importMainModule(scriptPath) {
    try {
        // Get entrypoint type from environment
        const entrypointType = process.env.ENTRYPOINT_TYPE || 'bot';
        const workDir = '/' + entrypointType;

        const botModule = require(workDir + '/' + scriptPath);
        const main = botModule.main || botModule.default || botModule;

        if (typeof main !== 'function') {
            throw new Error('No main function found in module');
        }
        return main;

    } catch (error) {
        console.error('IMPORT_ERROR: Import failed: ' + error.message);
        console.error('IMPORT_ERROR: Stack trace: ' + error.stack);
        writeResult({"status": "error", "message": error.message});
        process.exit(1);
    }
}

function parseConfig() {
    const botId = process.env.BOT_ID || '';
    const scriptPath = process.env.SCRIPT_PATH || 'main';
    const config = JSON.parse(process.env.BOT_CONFIG || '{}');
    console.error('CONFIG_SUCCESS: Bot ID: ' + botId);
    return { botId, config, scriptPath };
}

function main() {
    // Setup environment
    setupEnvironment();

    // Parse configuration
    const { botId, config, scriptPath } = parseConfig();

    // Import main module
    const mainFunc = importMainModule(scriptPath);

    // Execute the main function with graceful shutdown support
    Promise.resolve(mainFunc(botId, config))
        .then(result => {
            const output = result || {"status": "success", "message": "Bot executed successfully"};
            writeResult(output);
            process.exit(0);
        })
        .catch(error => {
            console.error('EXECUTE_ERROR: Bot execution failed: ' + error.message, error.stack);
            writeResult({ "status": "error", "message": error.message });
            process.exit(1);
        });
}

main();
`
