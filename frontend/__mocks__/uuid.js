// Mock for uuid
let counter = 0;

function v4() {
  return `mock-uuid-${counter++}`;
}

module.exports = { v4 };
module.exports.v4 = v4;
module.exports.default = { v4 };
