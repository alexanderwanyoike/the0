import { Injectable } from "@nestjs/common";
import { and, count, eq, ne, sql, type AnyColumn } from "drizzle-orm";
import { getDatabase, getDatabaseConfig } from "@/database/connection";
import { usersTable, usersTableSqlite } from "@/database/schema/users";
import { USER_ROLES } from "./user.constants";
import {
  CreateAdminUserInput,
  UpdateAdminUserInput,
  UserRecord,
} from "./user.types";

type UserUpdate = Record<string, unknown>;
interface CreateFirstAdminInput {
  username: string;
  email: string;
  firstName?: string;
  lastName?: string;
}

@Injectable()
export class UserRepository {
  private getUserTable() {
    const config = getDatabaseConfig();
    return config.type === "sqlite" ? usersTableSqlite : usersTable;
  }

  async count(): Promise<number> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const rows = (await db.select({ value: count() }).from(userTable)) as {
      value: number;
    }[];
    return Number(rows[0]?.value ?? 0);
  }

  async countActiveAdmins(): Promise<number> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const rows = (await db
      .select({ value: count() })
      .from(userTable)
      .where(
        and(eq(userTable.isActive, true), eq(userTable.role, USER_ROLES.ADMIN)),
      )) as { value: number }[];
    return Number(rows[0]?.value ?? 0);
  }

  async hasActiveAdmin(): Promise<boolean> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const rows = (await db
      .select({ id: userTable.id })
      .from(userTable)
      .where(
        and(eq(userTable.isActive, true), eq(userTable.role, USER_ROLES.ADMIN)),
      )
      .limit(1)) as { id: string }[];
    return rows.length > 0;
  }

  async list(): Promise<UserRecord[]> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    return (await db.select().from(userTable)) as UserRecord[];
  }

  async findById(id: string): Promise<UserRecord | null> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const users = (await db
      .select()
      .from(userTable)
      .where(eq(userTable.id, id))) as UserRecord[];
    return users[0] ?? null;
  }

  async findByEmail(email: string): Promise<UserRecord | null> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const users = (await db
      .select()
      .from(userTable)
      .where(eq(userTable.email, email))) as UserRecord[];
    return users[0] ?? null;
  }

  async findByUsername(username: string): Promise<UserRecord | null> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const users = (await db
      .select()
      .from(userTable)
      .where(eq(userTable.username, username))) as UserRecord[];
    return users[0] ?? null;
  }

  async findAnotherByUsername(
    username: string,
    excludedUserId: string,
  ): Promise<UserRecord | null> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const users = (await db
      .select()
      .from(userTable)
      .where(
        and(eq(userTable.username, username), ne(userTable.id, excludedUserId)),
      )) as UserRecord[];
    return users[0] ?? null;
  }

  async createFirstAdmin(
    credentials: CreateFirstAdminInput,
    passwordHash: string,
  ): Promise<UserRecord> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const users = (await db
      .insert(userTable)
      .values({
        username: credentials.username.trim(),
        email: credentials.email.trim(),
        passwordHash,
        firstName: credentials.firstName,
        lastName: credentials.lastName,
        role: USER_ROLES.ADMIN,
        isActive: true,
        isEmailVerified: true,
      })
      .returning()) as UserRecord[];
    return users[0];
  }

  async createUser(
    input: CreateAdminUserInput,
    username: string,
    email: string,
    passwordHash: string,
  ): Promise<UserRecord> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const users = (await db
      .insert(userTable)
      .values({
        username,
        email,
        passwordHash,
        firstName: input.firstName,
        lastName: input.lastName,
        role: input.role || USER_ROLES.USER,
        isActive: input.isActive ?? true,
        isEmailVerified: true,
      })
      .returning()) as UserRecord[];
    return users[0];
  }

  async update(id: string, input: UpdateAdminUserInput): Promise<UserRecord> {
    const updates: UserUpdate = { updatedAt: new Date() };
    if (input.username !== undefined) updates.username = input.username;
    if (input.email !== undefined) updates.email = input.email;
    if (input.firstName !== undefined) updates.firstName = input.firstName;
    if (input.lastName !== undefined) updates.lastName = input.lastName;
    if (input.role !== undefined) updates.role = input.role;
    if (input.isActive !== undefined) updates.isActive = input.isActive;

    return this.updateRaw(id, updates);
  }

  async updateProfile(
    id: string,
    input: {
      username?: string;
      firstName?: string;
      lastName?: string;
    },
  ): Promise<UserRecord> {
    const updates: UserUpdate = { updatedAt: new Date() };
    if (input.username !== undefined) updates.username = input.username;
    if (input.firstName !== undefined) updates.firstName = input.firstName;
    if (input.lastName !== undefined) updates.lastName = input.lastName;
    return this.updateRaw(id, updates);
  }

  async updateLastLogin(id: string): Promise<void> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    await db
      .update(userTable)
      .set({ lastLoginAt: new Date() })
      .where(eq(userTable.id, id));
  }

  async updatePassword(id: string, passwordHash: string): Promise<UserRecord> {
    return this.updateRaw(id, {
      passwordHash,
      sessionVersion: sqlIncrement(this.getUserTable().sessionVersion),
      updatedAt: new Date(),
    });
  }

  async deactivate(id: string): Promise<void> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    await db
      .update(userTable)
      .set({
        isActive: false,
        sessionVersion: sqlIncrement(userTable.sessionVersion),
        updatedAt: new Date(),
      })
      .where(eq(userTable.id, id));
  }

  async promoteToAdmin(id: string): Promise<void> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    await db
      .update(userTable)
      .set({ role: USER_ROLES.ADMIN, updatedAt: new Date() })
      .where(eq(userTable.id, id));
  }

  private async updateRaw(
    id: string,
    updates: UserUpdate,
  ): Promise<UserRecord> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const users = (await db
      .update(userTable)
      .set(updates)
      .where(eq(userTable.id, id))
      .returning()) as UserRecord[];
    return users[0];
  }
}

function sqlIncrement(column: AnyColumn) {
  return sql`${column} + 1`;
}
