import { Result } from './result';

export default interface Repository {
  create: (args: any) => Promise<Result<any, string>>;
  findAll: (userId: string) => Promise<Result<any[], string>>;
  findOne: (userId: string, id: string) => Promise<Result<any, string>>;
  update: (
    userId: string,
    id: string,
    args: any,
  ) => Promise<Result<any, string>>;
  remove: (userId: string, id: string) => Promise<Result<void, string>>;
}