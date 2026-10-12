/** Plays the script of the sdk/contract case in case.json through the0-node. */

const { loadCase, runSteps } = require('./contract-steps');

exports.main = async function () {
    await runSteps(loadCase().script || []);
};
