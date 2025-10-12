import { Injectable, Scope } from "@nestjs/common";
import { JwtService } from "@nestjs/jwt";
import { getDatabase } from "../database/connection";
import { usersTable, usersTableSqlite } from "../database/schema/users";
import { eq } from "drizzle-orm";
import * as bcrypt from "bcrypt";
import { Result } from "../common/result";
import { getDatabaseConfig } from "../database/connection";

export interface AuthUser {
  id: string;
  username: string;
  email: string;
  firstName?: string;
  lastName?: string;
  isActive: boolean;
  isEmailVerified: boolean;
}

export interface LoginCredentials {
  email: string;
  password: string;
}

export interface RegisterCredentials {
  username: string;
  email: string;
  password: string;
  firstName?: string;
  lastName?: string;
}

@Injectable({ scope: Scope.DEFAULT })
export class AuthService {
  constructor(private jwtService: JwtService) {}

  async validateToken(token: string): Promise<Result<AuthUser, string>> {
    try {
      const payload = this.jwtService.verify(token);
      const db = getDatabase();
      const config = getDatabaseConfig();

      const userTable =
        config.type === "sqlite" ? usersTableSqlite : usersTable;
      const users = await db
        .select()
        .from(userTable)
        .where(eq(userTable.id, payload.sub));

      if (users.length === 0) {
        return {
          success: false,
          error: "User not found",
          data: null,
        };
      }

      const user = users[0];
      if (!user.isActive) {
        return {
          success: false,
          error: "User account is inactive",
          data: null,
        };
      }

      return {
        success: true,
        error: null,
        data: {
          id: user.id,
          username: user.username,
          email: user.email,
          firstName: user.firstName,
          lastName: user.lastName,
          isActive: user.isActive,
          isEmailVerified: user.isEmailVerified,
        },
      };
    } catch (error) {
      return {
        success: false,
        error: "Invalid token",
        data: null,
      };
    }
  }

  async login(
    credentials: LoginCredentials,
  ): Promise<Result<{ token: string; user: AuthUser }, string>> {
    try {
      const db = getDatabase();
      const config = getDatabaseConfig();

      const userTable =
        config.type === "sqlite" ? usersTableSqlite : usersTable;
      const users = await db
        .select()
        .from(userTable)
        .where(eq(userTable.email, credentials.email));

      if (users.length === 0) {
        return {
          success: false,
          error: "Invalid credentials",
          data: null,
        };
      }

      const user = users[0];
      if (!user.isActive) {
        return {
          success: false,
          error: "User account is inactive",
          data: null,
        };
      }

      const isPasswordValid = await bcrypt.compare(
        credentials.password,
        user.passwordHash,
      );
      if (!isPasswordValid) {
        return {
          success: false,
          error: "Invalid credentials",
          data: null,
        };
      }

      // Update last login
      await db
        .update(userTable)
        .set({ lastLoginAt: new Date() })
        .where(eq(userTable.id, user.id));

      const payload = {
        sub: user.id,
        username: user.username,
        email: user.email,
      };

      const token = this.jwtService.sign(payload);

      return {
        success: true,
        error: null,
        data: {
          token,
          user: {
            id: user.id,
            username: user.username,
            email: user.email,
            firstName: user.firstName,
            lastName: user.lastName,
            isActive: user.isActive,
            isEmailVerified: user.isEmailVerified,
          },
        },
      };
    } catch (error) {
      return {
        success: false,
        error: "Login failed",
        data: null,
      };
    }
  }

  async register(
    credentials: RegisterCredentials,
  ): Promise<Result<{ token: string; user: AuthUser }, string>> {
    try {
      const db = getDatabase();
      const config = getDatabaseConfig();

      const userTable =
        config.type === "sqlite" ? usersTableSqlite : usersTable;

      // Check if user already exists
      const existingUsers = await db
        .select()
        .from(userTable)
        .where(eq(userTable.email, credentials.email));

      if (existingUsers.length > 0) {
        return {
          success: false,
          error: "User already exists",
          data: null,
        };
      }

      // Hash password
      const passwordHash = await bcrypt.hash(credentials.password, 10);

      // Create user
      const newUsers = await db
        .insert(userTable)
        .values({
          username: credentials.username,
          email: credentials.email,
          passwordHash,
          firstName: credentials.firstName,
          lastName: credentials.lastName,
          isActive: true,
          isEmailVerified: false,
        })
        .returning();

      const newUser = newUsers[0];

      const payload = {
        sub: newUser.id,
        username: newUser.username,
        email: newUser.email,
      };

      const token = this.jwtService.sign(payload);

      return {
        success: true,
        error: null,
        data: {
          token,
          user: {
            id: newUser.id,
            username: newUser.username,
            email: newUser.email,
            firstName: newUser.firstName,
            lastName: newUser.lastName,
            isActive: newUser.isActive,
            isEmailVerified: newUser.isEmailVerified,
          },
        },
      };
    } catch (error) {
      return {
        success: false,
        error: "Registration failed",
        data: null,
      };
    }
  }
}
