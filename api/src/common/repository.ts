import { Result } from "./result";

export default interface Repository<T = unknown> {
  create: (args: Partial<T>) => Promise<Result<T, string>>;
  findAll: (userId: string) => Promise<Result<T[], string>>;
  findOne: (userId: string, id: string) => Promise<Result<T, string>>;
  update: (
    userId: string,
    id: string,
    args: Partial<T>,
  ) => Promise<Result<T, string>>;
  remove: (userId: string, id: string) => Promise<Result<void, string>>;
}
