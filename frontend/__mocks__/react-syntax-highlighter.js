// Mock for react-syntax-highlighter
const { createElement } = require("react");

const SyntaxHighlighter = function ({ children, language, ...props }) {
  return createElement(
    "pre",
    {
      "data-language": language,
      ...props,
    },
    createElement("code", null, children),
  );
};

// Export all named exports that might be used
module.exports = SyntaxHighlighter;
module.exports.default = SyntaxHighlighter;
module.exports.Prism = SyntaxHighlighter;
module.exports.Light = SyntaxHighlighter;

// Mock styles
module.exports.vscDarkPlus = {};
module.exports.vs = {};
