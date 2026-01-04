/**
 * the0 Query Module - Express-like handler interface for bot queries.
 *
 * This module provides query handling capabilities for bots, allowing users to define
 * custom read-only query handlers that can be executed on demand.
 *
 * Separate namespace from state and main SDK exports.
 * Users import: import { query } from '@the0/sdk';
 *
 * @example
 * import { query, state } from '@the0/sdk';
 *
 * query.handler('/portfolio', async (req) => {
 *   const positions = await state.get('positions', []);
 *   return { positions, count: positions.length };
 * });
 *
 * query.handler('/status', (req) => {
 *   const symbol = req.get('symbol', 'BTC/USD');
 *   return { symbol, active: true };
 * });
 *
 * query.run();
 */

import * as http from "http";
import { URL } from "url";

/**
 * Request object passed to query handlers (Express-like).
 */
export interface QueryRequest {
  /** The query path being requested (e.g., "/portfolio") */
  path: string;
  /** Dictionary of query parameters */
  params: Record<string, string>;
  /**
   * Get a parameter value with optional default.
   * @param key - Parameter key to retrieve
   * @param defaultValue - Default value if parameter is not present
   * @returns Parameter value or default
   */
  get(key: string, defaultValue?: string): string | undefined;
}

/**
 * Query handler function type.
 */
export type QueryHandler = (
  req: QueryRequest
) => unknown | Promise<unknown>;

/**
 * Error thrown when attempting to modify state during query execution.
 */
export class ReadOnlyStateError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "ReadOnlyStateError";
  }
}

// Handler registry - maps paths to handler functions
const handlers = new Map<string, QueryHandler>();

// Query parameters for current request
let currentParams: Record<string, string> = {};

// Bot config loaded from environment
let config: Record<string, unknown> = {};

/**
 * Register a query handler (Express-like).
 *
 * @param path - The query path to handle (e.g., "/portfolio", "/signals")
 * @param fn - Handler function that receives request and returns response data
 *
 * @example
 * query.handler('/portfolio', async (req) => {
 *   const symbol = req.get('symbol');
 *   const positions = await state.get('positions', []);
 *   return { positions, symbol };
 * });
 *
 * query.handler('/health', (req) => {
 *   return { status: 'ok', uptime: process.uptime() };
 * });
 */
export function handler(path: string, fn: QueryHandler): void {
  handlers.set(path, fn);
}

/**
 * Get current query parameters (alternative to request object).
 *
 * @returns Copy of the current query parameters
 *
 * @example
 * query.handler('/example', (req) => {
 *   // Both approaches work:
 *   const params = query.getParams();
 *   const symbol = params.symbol;
 *   // Or use request object:
 *   const symbol2 = req.get('symbol');
 * });
 */
export function getParams(): Record<string, string> {
  return { ...currentParams };
}

/**
 * Get the bot configuration.
 *
 * @returns Copy of the bot configuration
 *
 * @example
 * query.handler('/status', (req) => {
 *   const cfg = query.getConfig();
 *   return { symbol: cfg.symbol, interval: cfg.interval };
 * });
 */
export function getConfig(): Record<string, unknown> {
  return { ...config };
}

/**
 * Run the query system with automatic mode detection.
 *
 * Modes:
 * - QUERY_PATH env set: Ephemeral mode (execute once, output JSON, exit)
 * - BOT_TYPE=realtime: Server mode (HTTP server on port 9476)
 * - Neither: Info mode (print available handlers)
 *
 * Environment Variables:
 * - QUERY_PATH: Path of query to execute (ephemeral mode)
 * - QUERY_PARAMS: JSON string of query parameters
 * - BOT_TYPE: Set to "realtime" to run as HTTP server
 * - THE0_QUERY_PORT: HTTP server port (default: 9476)
 * - BOT_CONFIG: JSON string of bot configuration
 *
 * @example
 * // In query.ts
 * import { query, state } from '@the0/sdk';
 *
 * query.handler('/portfolio', async (req) => {
 *   return { positions: await state.get('positions', []) };
 * });
 *
 * query.run();  // Automatically detects mode
 */
export async function run(): Promise<void> {
  // Load bot config from environment
  const configStr = process.env.BOT_CONFIG || "{}";
  try {
    config = JSON.parse(configStr);
  } catch {
    config = {};
  }

  // Register built-in handlers
  if (!handlers.has("/health")) {
    handlers.set("/health", () => ({ status: "ok" }));
  }
  if (!handlers.has("/info")) {
    handlers.set("/info", () => ({
      available_queries: Array.from(handlers.keys()),
    }));
  }

  const queryPath = process.env.QUERY_PATH;
  const botType = process.env.BOT_TYPE;

  if (queryPath) {
    await runEphemeral(queryPath);
  } else if (botType === "realtime") {
    await runServer();
  } else {
    await runEphemeral("/info");
  }
}

/**
 * Execute a single query and output JSON to stdout.
 */
async function runEphemeral(queryPath: string): Promise<void> {
  // Parse parameters from environment
  const paramsStr = process.env.QUERY_PARAMS || "{}";
  try {
    currentParams = JSON.parse(paramsStr);
  } catch {
    currentParams = {};
  }

  // Find handler
  const fn = handlers.get(queryPath);
  if (!fn) {
    console.log(
      JSON.stringify({
        status: "error",
        error: `No handler for path: ${queryPath}`,
        available: Array.from(handlers.keys()),
      })
    );
    process.exit(1);
  }

  try {
    const req = createRequest(queryPath, currentParams);
    const result = await fn(req);
    console.log(JSON.stringify({ status: "ok", data: result }));
  } catch (e) {
    console.log(
      JSON.stringify({ status: "error", error: String(e) })
    );
    process.exit(1);
  }
}

/**
 * Start HTTP server on port 9476 for realtime bots.
 */
async function runServer(): Promise<void> {
  const port = parseInt(process.env.THE0_QUERY_PORT || "9476", 10);

  const server = http.createServer(async (req, res) => {
    const url = new URL(req.url || "/", `http://localhost:${port}`);
    const path = url.pathname;
    currentParams = Object.fromEntries(url.searchParams);

    const fn = handlers.get(path);
    if (!fn) {
      sendJson(res, 404, {
        status: "error",
        error: `No handler for path: ${path}`,
      });
      return;
    }

    try {
      const request = createRequest(path, currentParams);
      const result = await fn(request);
      sendJson(res, 200, { status: "ok", data: result });
    } catch (e) {
      sendJson(res, 500, { status: "error", error: String(e) });
    }
  });

  server.listen(port, "0.0.0.0", () => {
    console.error(
      JSON.stringify({
        _log: "info",
        message: `Query server started on port ${port}`,
      })
    );
  });
}

/**
 * Create a QueryRequest object.
 */
function createRequest(
  path: string,
  params: Record<string, string>
): QueryRequest {
  return {
    path,
    params,
    get(key: string, defaultValue?: string): string | undefined {
      return params[key] ?? defaultValue;
    },
  };
}

/**
 * Send JSON response.
 */
function sendJson(
  res: http.ServerResponse,
  status: number,
  data: unknown
): void {
  res.writeHead(status, { "Content-Type": "application/json" });
  res.end(JSON.stringify(data));
}

/**
 * Check if currently running in query mode (read-only).
 * Used by state module to enforce read-only behavior.
 */
export function isQueryMode(): boolean {
  return process.env.QUERY_PATH !== undefined;
}
