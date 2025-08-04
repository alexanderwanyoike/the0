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

export { Failure, Ok };
