// Mock for react-markdown
module.exports = function Markdown({ children }) {
  return typeof children === "string" ? children : String(children);
};
