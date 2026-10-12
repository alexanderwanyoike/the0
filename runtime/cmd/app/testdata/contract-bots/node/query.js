/** Answers the handlers of the sdk/contract case in case.json through the0-node. */

const { query } = require('the0-node');
const { loadCase, runSteps } = require('./contract-steps');

for (const [path, steps] of Object.entries(loadCase().handlers || {})) {
    query.handler(path, (request) => runSteps(steps, request));
}

query.run().catch((e) => {
    console.error(e);
    process.exit(1);
});
