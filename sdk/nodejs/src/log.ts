// A module of its own so query.ts can log without importing index.ts, which
// imports query.ts.

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
