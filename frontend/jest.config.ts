// jest.config.ts
import type { Config } from 'jest';
import nextJest from 'next/jest';

const createJestConfig = nextJest({
  dir: './',
});

const customJestConfig: Config = {
  setupFilesAfterEnv: ['<rootDir>/jest.setup.ts'],
  testEnvironment: 'jest-fixed-jsdom',
  moduleNameMapper: {
    '^@/(.*)$': '<rootDir>/src/$1',
  },
  testPathIgnorePatterns: ['<rootDir>/.next/', '<rootDir>/node_modules/'],

  transformIgnorePatterns: [
    '/node_modules/(?!(string-width|strip-ansi|ansi-regex|cliui))',
  ],
  testEnvironmentOptions: {
    customExportConditions: [''],
  },
};

export default createJestConfig(customJestConfig);
