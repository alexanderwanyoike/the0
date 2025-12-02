import { PinoLogger } from "nestjs-pino";

/**
 * Creates a mock PinoLogger for use in tests
 */
export const createMockLogger = () => ({
  info: jest.fn(),
  error: jest.fn(),
  warn: jest.fn(),
  debug: jest.fn(),
  trace: jest.fn(),
  fatal: jest.fn(),
  setContext: jest.fn(),
  assign: jest.fn(),
  logger: {} as any,
  context: "TestContext",
  contextName: "TestContext",
  errorKey: "err",
  call: jest.fn(),
});

/**
 * Pre-created mock logger instance for simple test cases
 */
export const mockPinoLogger = createMockLogger();
