/**
 * What went wrong, from a fixed set every front door knows how to answer.
 * Add a code only when a caller has to react to it differently from the
 * existing ones.
 */
export type ErrorCode =
  | "validation"
  | "forbidden"
  | "not_found"
  /** The change lost a race with another one: retrying may succeed. */
  | "conflict"
  | "too_large"
  /** Understood and allowed, but the target cannot answer it. */
  | "unprocessable"
  | "internal"
  /** Something the api depends on cannot be reached. */
  | "unavailable"
  | "timeout";

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
