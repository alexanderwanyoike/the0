/**
 * the0 SDK for Node.js/TypeScript Trading Bots
 * =============================================
 *
 * This SDK provides utilities for building trading bots on the0 platform.
 *
 * @example
 * ```typescript
 * import { parse, success, error, result, metric } from '@the0/sdk';
 *
 * // Parse bot configuration
 * const { id, config } = parse();
 *
 * // Your trading logic here
 * console.log(`Bot ${id} trading ${config.symbol}`);
 *
 * // Signal completion
 * success('Trade executed successfully');
 * ```
 */

import * as fs from 'fs';
import * as path from 'path';

/**
 * Bot configuration parsed from environment variables
 */
export interface BotInput<T = Record<string, unknown>> {
  /** Unique bot instance ID */
  id: string;
  /** Bot configuration from the user */
  config: T;
}

/**
 * Result structure for bot execution
 */
export interface BotResult {
  status: 'success' | 'error';
  message: string;
  data?: Record<string, unknown>;
}

/**
 * Get the path to the result file
 */
function getResultFilePath(): string {
  const mountDir = process.env.CODE_MOUNT_DIR || 'bot';
  return `/${mountDir}/result.json`;
}

/**
 * Write result to the result file
 */
function writeResult(result: BotResult): void {
  try {
    const resultPath = getResultFilePath();
    fs.writeFileSync(resultPath, JSON.stringify(result));
  } catch (err) {
    // Fallback to stdout if file write fails
    console.error(`RESULT_ERROR: Failed to write result file: ${err}`);
  }
}

/**
 * Parse bot configuration from environment variables.
 *
 * Reads BOT_ID and BOT_CONFIG from the environment and returns
 * a typed configuration object.
 *
 * @returns Bot input containing id and config
 * @throws Error if environment variables are not set or config is invalid JSON
 *
 * @example
 * ```typescript
 * // Basic usage
 * const { id, config } = parse();
 *
 * // With typed config
 * interface MyConfig {
 *   symbol: string;
 *   amount: number;
 *   paper?: boolean;
 * }
 * const { id, config } = parse<MyConfig>();
 * console.log(config.symbol); // Type-safe access
 * ```
 */
export function parse<T = Record<string, unknown>>(): BotInput<T> {
  const id = process.env.BOT_ID;
  const configStr = process.env.BOT_CONFIG;

  if (!id) {
    throw new Error('BOT_ID environment variable not set');
  }

  if (!configStr) {
    throw new Error('BOT_CONFIG environment variable not set');
  }

  let config: T;
  try {
    config = JSON.parse(configStr) as T;
  } catch (err) {
    throw new Error(`Failed to parse BOT_CONFIG as JSON: ${err}`);
  }

  return { id, config };
}

/**
 * Output a success result.
 *
 * Writes a JSON result with status "success" and exits normally.
 *
 * @param message - Success message to include in the result
 * @param data - Optional additional data to include
 *
 * @example
 * ```typescript
 * // Simple success
 * success('Trade completed');
 *
 * // Success with data
 * success('Trade completed', {
 *   tradeId: '12345',
 *   filledAmount: 0.5,
 *   price: 45000
 * });
 * ```
 */
export function success(message: string, data?: Record<string, unknown>): void {
  writeResult({
    status: 'success',
    message,
    ...(data && { data }),
  });
}

/**
 * Output an error result and exit with code 1.
 *
 * Writes a JSON result with status "error" and terminates the process.
 *
 * @param message - Error message to include in the result
 * @param data - Optional additional error context
 *
 * @example
 * ```typescript
 * // Simple error
 * error('Failed to connect to exchange');
 *
 * // Error with context
 * error('Trade failed', {
 *   errorCode: 'INSUFFICIENT_FUNDS',
 *   available: 100,
 *   required: 150
 * });
 * ```
 */
export function error(message: string, data?: Record<string, unknown>): never {
  writeResult({
    status: 'error',
    message,
    ...(data && { data }),
  });
  process.exit(1);
}

/**
 * Output a custom result object.
 *
 * Use this when you need full control over the result structure.
 *
 * @param resultData - Custom result object (should include status)
 *
 * @example
 * ```typescript
 * result({
 *   status: 'success',
 *   message: 'Analysis complete',
 *   signals: [
 *     { symbol: 'BTC/USD', direction: 'long', confidence: 0.85 }
 *   ],
 *   timestamp: new Date().toISOString()
 * });
 * ```
 */
export function result(resultData: Record<string, unknown>): void {
  try {
    const resultPath = getResultFilePath();
    fs.writeFileSync(resultPath, JSON.stringify(resultData));
  } catch (err) {
    console.error(`RESULT_ERROR: Failed to write result file: ${err}`);
  }
}

/**
 * Emit a metric for the platform to collect.
 *
 * Metrics are logged as JSON with a special `_metric` field that
 * the platform recognizes and processes for dashboards and alerts.
 *
 * @param type - The metric type (e.g., 'price', 'signal', 'alert')
 * @param data - Metric data fields
 *
 * @example
 * ```typescript
 * // Emit a price metric
 * metric('price', {
 *   symbol: 'BTC/USD',
 *   value: 45000.50,
 *   change_pct: 2.5
 * });
 *
 * // Emit a trading signal
 * metric('signal', {
 *   symbol: 'ETH/USD',
 *   direction: 'long',
 *   confidence: 0.85,
 *   reason: 'MA crossover detected'
 * });
 *
 * // Emit an alert
 * metric('alert', {
 *   symbol: 'BTC/USD',
 *   type: 'price_spike',
 *   severity: 'high',
 *   message: 'Price increased 5% in 1 minute'
 * });
 * ```
 */
export function metric(type: string, data: Record<string, unknown>): void {
  console.log(
    JSON.stringify({
      _metric: type,
      ...data,
      timestamp: new Date().toISOString(),
    })
  );
}

/**
 * Log levels supported by the platform
 */
export type LogLevel = 'info' | 'warn' | 'error';

/**
 * Log a structured message to the bot's log output.
 *
 * Use this for debugging and monitoring. Messages appear in
 * the bot's log viewer in the platform as structured JSON.
 *
 * @param message - Message to log
 * @param dataOrLevel - Optional structured data object or log level
 * @param level - Optional log level (defaults to 'info')
 *
 * @example
 * ```typescript
 * // Simple log (defaults to info level)
 * log('Starting trade execution');
 *
 * // Log with level
 * log('Connection lost', 'warn');
 * log('Trade failed', 'error');
 *
 * // Log with structured data (pino-style)
 * log('Order placed', { orderId: '12345', symbol: 'BTC/USD' });
 *
 * // Log with data and level
 * log('Order failed', { orderId: '12345', reason: 'insufficient funds' }, 'error');
 * ```
 */
export function log(
  message: string,
  dataOrLevel?: Record<string, unknown> | LogLevel,
  level?: LogLevel
): void {
  let data: Record<string, unknown> | undefined;
  let logLevel: LogLevel = 'info';

  if (typeof dataOrLevel === 'string') {
    logLevel = dataOrLevel;
  } else if (dataOrLevel) {
    data = dataOrLevel;
    logLevel = level || 'info';
  }

  const entry = {
    level: logLevel,
    message,
    ...data,
    timestamp: new Date().toISOString(),
  };

  console.error(JSON.stringify(entry));
}

/**
 * Sleep utility for async operations.
 *
 * @param ms - Milliseconds to sleep
 * @returns Promise that resolves after the specified time
 *
 * @example
 * ```typescript
 * // Wait 5 seconds between operations
 * await sleep(5000);
 * ```
 */
export function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

// Export state module as namespace
import * as state from './state';
export { state };

// Default export for convenience
export default {
  parse,
  success,
  error,
  result,
  metric,
  log,
  sleep,
  state,
};
