import { Injectable, ServiceUnavailableException } from "@nestjs/common";
import { JwtService } from "@nestjs/jwt";
import * as bcrypt from "bcrypt";
import { isConnectionError } from "@/common/database-errors";
import { Failure, Result } from "../common/result";
import { UserRole } from "@/user/user.constants";
import { UserRepository } from "@/user/user.repository";
import { UserRecord } from "@/user/user.types";
import { toAuthUser } from "./auth-user.mapper";
import { AuthUser } from "./auth.types";

export type { AuthUser } from "./auth.types";

export interface LoginCredentials {
  email: string;
  password: string;
}

interface JwtPayload {
  sub: string;
  username?: string;
  email?: string;
  role?: UserRole;
  sessionVersion?: number;
}

@Injectable()
export class AuthService {
  constructor(
    private jwtService: JwtService,
    private readonly users: UserRepository,
  ) {}

  private signUser(user: UserRecord): { token: string; user: AuthUser } {
    const authUser = toAuthUser(user);
    const token = this.jwtService.sign({
      sub: authUser.id,
      username: authUser.username,
      email: authUser.email,
      role: authUser.role,
      sessionVersion: user.sessionVersion,
    });

    return { token, user: authUser };
  }

  async validateToken(token: string): Promise<Result<AuthUser, string>> {
    try {
      const payload = this.jwtService.verify<JwtPayload>(token);
      const user = await this.users.findById(payload.sub);

      if (!user) {
        return {
          success: false,
          error: "User not found",
          data: null,
        };
      }

      if (!user.isActive) {
        return {
          success: false,
          error: "User account is inactive",
          data: null,
        };
      }

      if ((payload.sessionVersion ?? 0) !== (user.sessionVersion ?? 0)) {
        return {
          success: false,
          error: "Invalid token",
          data: null,
        };
      }

      return {
        success: true,
        error: null,
        data: toAuthUser(user),
      };
    } catch (error) {
      if (isConnectionError(error)) {
        throw new ServiceUnavailableException(
          "Database temporarily unavailable",
        );
      }
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
      const user = await this.users.findByEmail(credentials.email);

      if (!user) {
        return {
          success: false,
          error: "Invalid credentials",
          data: null,
        };
      }

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

      await this.users.updateLastLogin(user.id);

      return {
        success: true,
        error: null,
        data: this.signUser(user),
      };
    } catch (error) {
      if (isConnectionError(error)) {
        throw new ServiceUnavailableException(
          "Database temporarily unavailable",
        );
      }
      return {
        success: false,
        error: "Login failed",
        data: null,
      };
    }
  }
}
