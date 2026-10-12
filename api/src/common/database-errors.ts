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

// Postgres 18 reports an ON DELETE RESTRICT violation as restrict_violation
// (23001); earlier versions report it as foreign_key_violation (23503).
const FOREIGN_KEY_VIOLATION_CODES = new Set(["23503", "23001"]);

// Drizzle wraps driver errors in DrizzleQueryError, which keeps the driver's
// error, and with it the SQLSTATE, as `cause`.
export function isForeignKeyViolation(error: unknown): boolean {
  const { code, cause } = (error ?? {}) as {
    code?: unknown;
    cause?: { code?: unknown };
  };
  return [code, cause?.code].some(
    (c) => typeof c === "string" && FOREIGN_KEY_VIOLATION_CODES.has(c),
  );
}

export function isUniqueConstraintError(error: unknown): boolean {
  const code = (error as { code?: unknown })?.code;
  return (
    code === "23505" ||
    code === "SQLITE_CONSTRAINT" ||
    code === "SQLITE_CONSTRAINT_PRIMARYKEY" ||
    code === "SQLITE_CONSTRAINT_UNIQUE"
  );
}
