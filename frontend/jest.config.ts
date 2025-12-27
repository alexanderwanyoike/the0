// jest.config.ts
import type { Config } from "jest";
import nextJest from "next/jest";

const createJestConfig = nextJest({
  dir: "./",
});

const customJestConfig: Config = {
  setupFilesAfterEnv: ["<rootDir>/jest.setup.ts"],
  testEnvironment: "jest-fixed-jsdom",
  collectCoverageFrom: [
    "src/**/*.{ts,tsx}",
    "!src/**/*.d.ts",
    "!src/**/__tests__/**",
    "!src/**/types/**",
  ],
  coverageThreshold: {
    global: {
      // No global threshold - focus on per-file thresholds for bot frontend feature
      branches: 0,
      functions: 0,
      lines: 0,
      statements: 0,
    },
    // Bot frontend feature - core files
    "src/components/bot/*.tsx": {
      branches: 40,
      functions: 60,
      lines: 60,
      statements: 60,
    },
    "src/contexts/bot-events-context.tsx": {
      branches: 80,
      functions: 80,
      lines: 80,
      statements: 80,
    },
    "src/hooks/use-bot-events.ts": {
      branches: 80,
      functions: 80,
      lines: 80,
      statements: 80,
    },
    "src/lib/events/event-parser.ts": {
      branches: 90,
      functions: 90,
      lines: 90,
      statements: 90,
    },
    "src/lib/events/event-utils.ts": {
      branches: 90,
      functions: 90,
      lines: 90,
      statements: 90,
    },
  },
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
