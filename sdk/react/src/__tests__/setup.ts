// Jest setup for @the0/react SDK
import "@testing-library/react";

// Suppress console errors during tests unless debugging
const originalError = console.error;
beforeAll(() => {
  console.error = (...args: unknown[]) => {
    // Suppress React 19 specific warnings and act() warnings in tests
    if (
      typeof args[0] === "string" &&
      (args[0].includes("ReactDOMTestUtils.act") ||
        args[0].includes("Warning: An update to"))
    ) {
      return;
    }
    originalError.call(console, ...args);
  };
});

afterAll(() => {
  console.error = originalError;
});
