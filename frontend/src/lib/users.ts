"use client";
import { Failure, Ok, Result } from "@/lib/result";
import { UserService, ApiUser } from "@/lib/api/user.service";
// Use ApiUser as UserData for compatibility
export type UserData = ApiUser;

export type UserRepositoryError = {
  error: string;
  status: number;
};

export class UserRepository {
  static async create(
    userId: string,
    email: string,
    displayName?: string | null,
  ): Promise<Result<UserData, UserRepositoryError>> {
    return await UserService.createUser({
      userId,
      email,
      displayName: displayName || undefined,
    });
  }

  static async get(
    userId: string,
  ): Promise<Result<UserData | null, UserRepositoryError>> {
    return await UserService.getUser(userId);
  }

  static async update(
    userId: string,
    data: Partial<UserData>,
  ): Promise<Result<null, UserRepositoryError>> {
    const result = await UserService.updateUser(userId, {
      displayName: data.displayName || undefined,
      photoURL: data.photoURL || undefined,
    });
    if (result.success) {
      return Ok(null);
    }
    return Failure(result.error!);
  }

  static async updateLastLogin(
    userId: string,
  ): Promise<Result<null, UserRepositoryError>> {
    return await UserService.updateLastLogin(userId);
  }

  static async deleteUser(
    userId: string,
  ): Promise<Result<null, UserRepositoryError>> {
    return await UserService.deleteUser(userId);
  }
}
