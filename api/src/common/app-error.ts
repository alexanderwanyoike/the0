import { Failure, Result } from "./result";

/**
 * What went wrong, from a fixed set every front door knows how to answer.
 * Add a code only when a caller has to react to it differently from the
 * existing ones.
 */
export type ErrorCode = "validation" | "forbidden" | "not_found" | "internal";

/**
 * The error every service and Bot command fails with. The message is for
 * people: callers branch on the code, never on the message.
 */
export interface AppError {
  readonly code: ErrorCode;
  readonly message: string;
}

export const appError = (code: ErrorCode, message: string): AppError => ({
  code,
  message,
});

export const invalid = <T>(message: string): Result<T, AppError> =>
  Failure(appError("validation", message));

// The driver's message can quote the values written, so it stays in the log.
export const READ_FAILED = appError(
  "internal",
  "Failed to read from the database",
);
export const WRITE_FAILED = appError(
  "internal",
  "Failed to write to the database",
);
