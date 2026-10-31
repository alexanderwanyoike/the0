/**
 * What went wrong, from a fixed set every front door knows how to answer.
 * Add a code only when a caller has to react to it differently from the
 * existing ones.
 */
export type ErrorCode = "validation" | "not_found" | "internal";

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
