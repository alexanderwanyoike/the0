import { User as PgUser, usersTableSqlite } from "@/database/schema/users";
import { UserRole } from "./user.constants";

export type SqliteUser = typeof usersTableSqlite.$inferSelect;
export type UserRecord = PgUser | SqliteUser;

export interface CreateAdminUserInput {
  username?: string;
  email: string;
  password: string;
  firstName?: string;
  lastName?: string;
  role?: UserRole;
  isActive?: boolean;
}

export interface UpdateAdminUserInput {
  username?: string;
  email?: string;
  firstName?: string | null;
  lastName?: string | null;
  role?: UserRole;
  isActive?: boolean;
}

export interface UpdateProfileInput {
  username?: string;
  firstName?: string;
  lastName?: string;
}

export interface SerializedUser {
  id: string;
  username: string;
  email: string;
  firstName?: string | null;
  lastName?: string | null;
  role: UserRole;
  isActive: boolean;
  isEmailVerified: boolean;
  isConfiguredRootAdmin?: boolean;
  lastLoginAt?: Date | null;
  createdAt: Date;
  updatedAt: Date;
}
