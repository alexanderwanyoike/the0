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

export { Failure, Ok, errorMessage };
