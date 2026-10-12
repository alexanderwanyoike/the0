/**
 * Runs the ops of the sdk/contract case in case.json through the0-node.
 *
 * This is the op interpreter of the Node.js SDK's contract bot
 * (sdk/nodejs/src/contract-bot.ts) in plain JavaScript. That file starts its
 * own main() when loaded, so a bot started by the runtime's wrapper cannot
 * reuse it; keep the two in step.
 */

const fs = require('fs');
const path = require('path');
const the0 = require('the0-node');

const { query, state } = the0;

const catchable = {
    read_only_state: (e) => e instanceof query.ReadOnlyStateError,
    any: () => true,
};

function loadCase() {
    return JSON.parse(fs.readFileSync(path.join(__dirname, 'case.json'), 'utf8'));
}

function resolve(value, bound) {
    if (Array.isArray(value)) {
        return value.map((item) => resolve(item, bound));
    }
    if (value !== null && typeof value === 'object') {
        const entries = Object.entries(value);
        if (entries.length === 1 && entries[0][0] === '$ref') {
            return bound[entries[0][1]];
        }
        return Object.fromEntries(entries.map(([key, item]) => [key, resolve(item, bound)]));
    }
    return value;
}

// Calls fn with the optional args the step gives, so an omitted one stays omitted.
function callWith(fn, args, required, optional = []) {
    const given = optional.filter((name) => name in args);
    return fn(...[...required, ...given].map((name) => args[name]));
}

async function perform(step, request, bound) {
    const args = {};
    for (const [key, value] of Object.entries(step)) {
        if (key !== 'op' && key !== 'as' && key !== 'catch') {
            args[key] = resolve(value, bound);
        }
    }

    switch (step.op) {
        case 'parse': {
            const { id, config } = the0.parse();
            return { bot_id: id, config };
        }
        case 'success':
            return callWith(the0.success, args, ['message'], ['data']);
        case 'error':
            return callWith(the0.error, args, ['message'], ['data']);
        case 'result':
            return the0.result(args.value);
        case 'metric':
            return the0.metric(args.type, args.data);
        case 'log':
            return callWith(the0.log, args, ['message'], ['data', 'level']);
        case 'raise':
            throw new Error(args.message);

        case 'state.get':
            return callWith(state.get, args, ['key'], ['default']);
        case 'state.set':
            return state.set(args.key, args.value);
        case 'state.delete':
            return state.delete(args.key);
        case 'state.list':
            return state.list();
        case 'state.exists':
            return state.exists(args.key);
        case 'state.clear':
            return state.clear();
    }

    if (request === null) {
        throw new Error('contract-steps: op ' + step.op + ' is only valid inside a query handler');
    }
    switch (step.op) {
        case 'query.params':
            return { ...request.params };
        case 'query.param':
            return callWith(request.get, args, ['key'], ['default']);
        case 'query.config':
            return query.getConfig();
    }
    throw new Error('contract-steps: unknown op ' + step.op);
}

async function runSteps(steps, request = null) {
    const bound = {};
    for (const step of steps) {
        if (step.op === 'return') {
            return resolve(step.value, bound);
        }
        let value;
        try {
            value = await perform(step, request, bound);
        } catch (e) {
            if (step.catch === undefined || !catchable[step.catch]?.(e)) {
                throw e;
            }
            value = { caught: step.catch };
        }
        if (step.as !== undefined) {
            bound[step.as] = value === undefined ? null : value;
        }
    }
    return null;
}

module.exports = { loadCase, runSteps };
