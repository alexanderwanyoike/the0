// jest.config.ts
import type { Config } from "jest";
import nextJest from "next/jest";

const createJestConfig = nextJest({
  dir: "./",
});

const customJestConfig: Config = {
  setupFilesAfterEnv: ["<rootDir>/jest.setup.ts"],
  testEnvironment: "jest-fixed-jsdom",
  moduleNameMapper: {
    "^@/(.*)$": "<rootDir>/src/$1",
    "^react-markdown$": "<rootDir>/__mocks__/react-markdown.js",
    "^remark-gfm$": "<rootDir>/__mocks__/remark-gfm.js",
    "^remark-breaks$": "<rootDir>/__mocks__/remark-breaks.js",
    "^react-syntax-highlighter$":
      "<rootDir>/__mocks__/react-syntax-highlighter.js",
    "^react-syntax-highlighter/dist/esm/(.*)$":
      "<rootDir>/__mocks__/react-syntax-highlighter.js",
    "^uuid$": "<rootDir>/__mocks__/uuid.js",
  },
  testPathIgnorePatterns: ["<rootDir>/.next/", "<rootDir>/node_modules/"],

  transformIgnorePatterns: [
    "/node_modules/(?!(string-width|strip-ansi|ansi-regex|cliui))",
  ],
  testEnvironmentOptions: {
    customExportConditions: [""],
  },
};

export default createJestConfig(customJestConfig);
