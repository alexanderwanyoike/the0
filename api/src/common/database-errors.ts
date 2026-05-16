const CONNECTION_ERROR_CODES = new Set([
  "ETIMEDOUT",
  "ENETUNREACH",
  "ECONNREFUSED",
  "ECONNRESET",
  "ENOTFOUND",
  "CONNECT_TIMEOUT",
]);

export function isConnectionError(error: unknown): boolean {
  if (error instanceof AggregateError) {
    return error.errors.some(isConnectionError);
  }
  const code = (error as { code?: unknown })?.code;
  return typeof code === "string" && CONNECTION_ERROR_CODES.has(code);
}
