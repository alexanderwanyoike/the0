package entrypoints

const NodeJsBotEntrypoint = `#!/usr/bin/env node
/**
 * Node.js Bot Entrypoint Script
 * Handles bot execution with signal management and graceful shutdown.
 */

const fs = require('fs');
const path = require('path');

// Result marker for output protocol - runtime parses this to extract results
const RESULT_MARKER = 'THE0_RESULT:';

const controller = new AbortController();

function handleSignal(signal) {
    console.error('SIGNAL: Received ' + signal + ', triggering abort.');
    // --- Trigger the abort signal ---
    controller.abort();
}

// Register signal handlers
process.on('SIGTERM', () => handleSignal('SIGTERM'));
process.on('SIGINT', () => handleSignal('SIGINT'));

console.error('STARTUP: Node.js bot wrapper starting');
console.error('STARTUP: Node.js version ' + process.version);
console.error('STARTUP: Current working directory: ' + process.cwd());
console.error('STARTUP: Signal handlers registered for SIGTERM and SIGINT');

function setupEnvironment() {
    // Get entrypoint type from environment
    const entrypointType = process.env.ENTRYPOINT_TYPE || 'bot';
    const workDir = '/' + entrypointType;
    
    // Change to work directory
    try {
        console.error('CHDIR_ATTEMPT: Changing to ' + workDir + ' directory');
        process.chdir(workDir);
        console.error('CHDIR_SUCCESS: Changed to working directory: ' + process.cwd());
    } catch (error) {
        console.error('CHDIR_ERROR: Could not change to ' + workDir + ' directory: ' + error.message);
        process.exit(1);
    }

    // Check for package.json
    const packagePath = workDir + '/package.json';
    if (fs.existsSync(packagePath)) {
        console.error('PACKAGE_CHECK: package.json found');
        try {
            const packageContent = fs.readFileSync(packagePath, 'utf8');
            const packageJson = JSON.parse(packageContent);
            console.error('PACKAGE_DEPS: ' + Object.keys(packageJson.dependencies || {}).join(', '));
        } catch (error) {
            console.error('PACKAGE_READ_ERROR: ' + error.message);
        }
    } else {
        console.error('PACKAGE_CHECK: no package.json found');
    }
}

function importMainModule(scriptPath) {
    console.error('IMPORT_ATTEMPT: Starting main module import');
    try {
        // Get entrypoint type from environment
        const entrypointType = process.env.ENTRYPOINT_TYPE || 'bot';
        const workDir = '/' + entrypointType;
        
        console.error('IMPORT_STEP: Requiring ' + workDir + '/' + scriptPath);
        const botModule = require(workDir + '/' + scriptPath);
        console.error('IMPORT_STEP: Module loaded successfully');
        
        const main = botModule.main || botModule.default || botModule;
        console.error('IMPORT_STEP: Main function type: ' + typeof main);
        
        if (typeof main !== 'function') {
            throw new Error('No main function found in module');
        }
        console.error('IMPORT_SUCCESS: Main function found');
        return main;
        
    } catch (error) {
        console.error('IMPORT_ERROR: Import failed: ' + error.message);
        console.error('IMPORT_ERROR: Stack trace: ' + error.stack);
        console.log(RESULT_MARKER + JSON.stringify({"status": "error", "message": error.message}));
        process.exit(1);
    }
}

function parseConfig() {
    console.error('CONFIG_PARSE: Parsing bot configuration');
    const botId = process.env.BOT_ID || '';
    const scriptPath = process.env.SCRIPT_PATH || 'main';
    const config = JSON.parse(process.env.BOT_CONFIG || '{}');
    console.error('CONFIG_SUCCESS: Bot ID: ' + botId);
    console.error('CONFIG_SUCCESS: Script path: ' + scriptPath);
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
    console.error('EXECUTE_ATTEMPT: Starting bot execution');
    Promise.resolve(mainFunc(botId, config))
        .then(result => {
            console.error('EXECUTE_SUCCESS: Bot execution completed on its own.');
            const output = result || {"status": "success", "message": "Bot executed successfully"};
            console.log(RESULT_MARKER + JSON.stringify(output));
            process.exit(0);
        })
        .catch(error => {
            console.error('EXECUTE_ERROR: Bot execution failed: ' + error.message, error.stack);
            console.log(RESULT_MARKER + JSON.stringify({ "status": "error", "message": error.message }));
            process.exit(1);
        });
}

main();
`
