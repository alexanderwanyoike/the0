export type Result<T, E> = Success<T> | Failure<E>;

export interface Success<T> {
  data: T;
  success: true;
}

export interface Failure<E> {
  error: E;
  success: false;
}

export const Ok = <T, E>(data: T): Result<T, E> => ({
  data,
  success: true,
});

export const Failure = <T, E>(error: E): Result<T, E> => ({
  error,
  success: false,
});
