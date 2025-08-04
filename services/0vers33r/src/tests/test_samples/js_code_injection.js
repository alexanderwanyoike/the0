// JavaScript Code Injection Test Sample
// This file contains patterns that should trigger JavaScript YARA rules

const userInput = process.argv[2] || 'alert("test")';

// Critical: eval() usage
const result = eval(userInput);

// Critical: Function constructor
const maliciousFunc = new Function('return ' + userInput)();

// Critical: VM execution
const vm = require('vm');
vm.runInNewContext(userInput);

// Suspicious: Dynamic imports with eval
const dynamicModule = import(eval('module_name'));

// Suspicious: Obfuscated code patterns
const obfuscated = String.fromCharCode(97, 108, 101, 114, 116);

// DOM manipulation with user input (XSS potential)
if (typeof document !== 'undefined') {
    document.getElementById('content').innerHTML = userInput;
}

console.log('Code injection test completed');