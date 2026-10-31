import { AppError } from "./app-error";
import { Result } from "./result";

export default interface Repository<T = unknown> {
  create: (args: Partial<T>) => Promise<Result<T, AppError>>;
  findAll: (userId: string) => Promise<Result<T[], AppError>>;
  findOne: (userId: string, id: string) => Promise<Result<T, AppError>>;
  update: (
    userId: string,
    id: string,
    args: Partial<T>,
  ) => Promise<Result<T, AppError>>;
  remove: (userId: string, id: string) => Promise<Result<void, AppError>>;
}
