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
const errorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/** Type guard for errors with a `code` property (e.g. MinIO S3 errors). */
const hasErrorCode = (error: unknown): error is { code: string } =>
  typeof error === "object" && error !== null && "code" in error;

export { Failure, Ok, errorMessage, hasErrorCode };
