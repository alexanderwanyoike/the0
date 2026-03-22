export type Result<T, E> = {
  data: T | null;
  error: E | null;
  success: boolean;
};

const Failure = <T, E>(error: E): Result<T, E> => ({
  data: null,
  error,
  success: false,
});

const Ok = <T, E>(data: T): Result<T, E> => ({
  data,
  error: null,
  success: true,
});

/**
 * Extract a message string from an unknown error value.
 * Prefer this over `(error as any).message` in catch blocks.
 */
const errorMessage = (error: unknown): string => {
  if (error instanceof Error) return error.message;
  if (
    typeof error === "object" &&
    error !== null &&
    "message" in error &&
    typeof (error as { message: unknown }).message === "string"
  ) {
    return (error as { message: string }).message;
  }
  return String(error);
};

/** Type guard for errors with a `code` property (e.g. MinIO S3 errors). */
const hasErrorCode = (error: unknown): error is { code: string } =>
  typeof error === "object" &&
  error !== null &&
  "code" in error &&
  typeof (error as { code: unknown }).code === "string";

export { Failure, Ok, errorMessage, hasErrorCode };
