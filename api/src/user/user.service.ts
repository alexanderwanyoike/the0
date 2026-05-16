import {
  BadRequestException,
  ForbiddenException,
  Injectable,
  NotFoundException,
  UnauthorizedException,
} from "@nestjs/common";
import { and, eq, ne } from "drizzle-orm";
import * as bcrypt from "bcrypt";
import { getDatabase, getDatabaseConfig } from "@/database/connection";
import { usersTable, usersTableSqlite } from "@/database/schema/users";
import { AuthenticatedUser } from "@/auth/auth.types";

type UserRole = "admin" | "user";

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

@Injectable()
export class UserService {
  private getUserTable() {
    const config = getDatabaseConfig();
    return config.type === "sqlite" ? usersTableSqlite : usersTable;
  }

  private serializeUser(user: any) {
    return {
      id: user.id,
      username: user.username,
      email: user.email,
      firstName: user.firstName,
      lastName: user.lastName,
      role: user.role === "admin" ? "admin" : "user",
      isActive: Boolean(user.isActive),
      isEmailVerified: Boolean(user.isEmailVerified),
      lastLoginAt: user.lastLoginAt,
      createdAt: user.createdAt,
      updatedAt: user.updatedAt,
    };
  }

  private defaultUsername(email: string): string {
    return email.split("@")[0] || email;
  }

  async listUsers() {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const users = await db.select().from(userTable);
    return users.map((user: any) => this.serializeUser(user));
  }

  async createUser(input: CreateAdminUserInput) {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const username = input.username || this.defaultUsername(input.email);

    const existingByEmail = await db
      .select()
      .from(userTable)
      .where(eq(userTable.email, input.email));
    if (existingByEmail.length > 0) {
      throw new BadRequestException("Email is already in use");
    }

    const existingByUsername = await db
      .select()
      .from(userTable)
      .where(eq(userTable.username, username));
    if (existingByUsername.length > 0) {
      throw new BadRequestException("Username is already in use");
    }

    const passwordHash = await bcrypt.hash(input.password, 10);
    const users = await db
      .insert(userTable)
      .values({
        username,
        email: input.email,
        passwordHash,
        firstName: input.firstName,
        lastName: input.lastName,
        role: input.role || "user",
        isActive: input.isActive ?? true,
        isEmailVerified: true,
      })
      .returning();

    return this.serializeUser(users[0]);
  }

  async updateUser(
    id: string,
    input: UpdateAdminUserInput,
    actor: AuthenticatedUser,
  ) {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const user = await this.requireUser(id);

    if (id === actor.id && input.role && input.role !== "admin") {
      throw new ForbiddenException("You cannot demote your own admin account");
    }
    if (id === actor.id && input.isActive === false) {
      throw new ForbiddenException(
        "You cannot deactivate your own admin account",
      );
    }

    await this.assertAdminWillRemain(user, input);

    if (input.email && input.email !== user.email) {
      const existing = await db
        .select()
        .from(userTable)
        .where(eq(userTable.email, input.email));
      if (existing.length > 0) {
        throw new BadRequestException("Email is already in use");
      }
    }

    if (input.username && input.username !== user.username) {
      const existing = await db
        .select()
        .from(userTable)
        .where(eq(userTable.username, input.username));
      if (existing.length > 0) {
        throw new BadRequestException("Username is already in use");
      }
    }

    const updates: Record<string, unknown> = {
      updatedAt: new Date(),
    };
    for (const key of ["username", "email", "firstName", "lastName", "role"]) {
      if ((input as any)[key] !== undefined) {
        updates[key] = (input as any)[key];
      }
    }
    if (input.isActive !== undefined) {
      updates.isActive = input.isActive;
    }

    const updated = await db
      .update(userTable)
      .set(updates)
      .where(eq(userTable.id, id))
      .returning();

    return this.serializeUser(updated[0]);
  }

  async resetPassword(id: string, password: string) {
    const db = getDatabase();
    const userTable = this.getUserTable();
    await this.requireUser(id);

    const passwordHash = await bcrypt.hash(password, 10);
    const updated = await db
      .update(userTable)
      .set({ passwordHash, updatedAt: new Date() })
      .where(eq(userTable.id, id))
      .returning();

    return this.serializeUser(updated[0]);
  }

  async updateProfile(
    actor: AuthenticatedUser,
    input: { username?: string; firstName?: string; lastName?: string },
  ) {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const updates: Record<string, unknown> = { updatedAt: new Date() };

    if (input.username) {
      const existing = await db
        .select()
        .from(userTable)
        .where(
          and(
            eq(userTable.username, input.username),
            ne(userTable.id, actor.id),
          ),
        );
      if (existing.length > 0) {
        throw new BadRequestException("Username is already in use");
      }
      updates.username = input.username;
    }
    if (input.firstName !== undefined) updates.firstName = input.firstName;
    if (input.lastName !== undefined) updates.lastName = input.lastName;

    const updated = await db
      .update(userTable)
      .set(updates)
      .where(eq(userTable.id, actor.id))
      .returning();

    return this.serializeUser(updated[0]);
  }

  async changePassword(
    actor: AuthenticatedUser,
    currentPassword: string,
    newPassword: string,
  ) {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const user = await this.requireUser(actor.id);

    const passwordMatches = await bcrypt.compare(
      currentPassword,
      user.passwordHash,
    );
    if (!passwordMatches) {
      throw new UnauthorizedException("Current password is incorrect");
    }

    const passwordHash = await bcrypt.hash(newPassword, 10);
    await db
      .update(userTable)
      .set({ passwordHash, updatedAt: new Date() })
      .where(eq(userTable.id, actor.id));

    return { success: true };
  }

  async deleteAccount(actor: AuthenticatedUser, password: string) {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const user = await this.requireUser(actor.id);

    const passwordMatches = await bcrypt.compare(password, user.passwordHash);
    if (!passwordMatches) {
      throw new UnauthorizedException("Password is incorrect");
    }

    await this.assertAdminWillRemain(user, { isActive: false });

    await db.delete(userTable).where(eq(userTable.id, actor.id));
    return { success: true };
  }

  private async requireUser(id: string) {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const users = await db.select().from(userTable).where(eq(userTable.id, id));
    if (users.length === 0) {
      throw new NotFoundException("User not found");
    }
    return users[0] as any;
  }

  private async assertAdminWillRemain(
    currentUser: any,
    input: { role?: UserRole; isActive?: boolean },
  ) {
    if (currentUser.role !== "admin" || !currentUser.isActive) {
      return;
    }

    const wouldRemoveAdmin = input.role === "user" || input.isActive === false;
    if (!wouldRemoveAdmin) {
      return;
    }

    const db = getDatabase();
    const userTable = this.getUserTable();
    const users = await db.select().from(userTable);
    const activeAdminCount = users.filter(
      (user: any) => user.isActive && user.role === "admin",
    ).length;

    if (activeAdminCount <= 1) {
      throw new ForbiddenException("At least one active admin is required");
    }
  }
}
