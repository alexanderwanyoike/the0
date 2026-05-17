import { Injectable, ServiceUnavailableException } from "@nestjs/common";
import { JwtService } from "@nestjs/jwt";
import * as bcrypt from "bcrypt";
import { isConnectionError } from "@/common/database-errors";
import { Failure, Result } from "../common/result";
import { hashPassword } from "@/common/password";
import { UserRole } from "@/user/user.constants";
import { UserRepository } from "@/user/user.repository";
import { UserRecord } from "@/user/user.types";
import { toAuthUser } from "./auth-user.mapper";
import { AuthUser } from "./auth.types";
import { SetupLockRepository } from "./setup-lock.repository";

export type { AuthUser } from "./auth.types";

export interface LoginCredentials {
  email: string;
  password: string;
}

export interface SetupCredentials {
  username: string;
  email: string;
  password: string;
  firstName?: string;
  lastName?: string;
}

export interface SetupStatus {
  setupRequired: boolean;
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
    private readonly setupLocks: SetupLockRepository,
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

  private getConfiguredAdminEmail(): string | undefined {
    const email = process.env.THE0_ADMIN_EMAIL?.trim();
    return email || undefined;
  }

  async getSetupStatus(): Promise<SetupStatus> {
    const userCount = await this.users.count();

    return {
      setupRequired: userCount === 0,
    };
  }

  async createFirstAdmin(
    credentials: SetupCredentials,
  ): Promise<Result<{ token: string; user: AuthUser }, string>> {
    try {
      const userCount = await this.users.count();

      if (userCount > 0) {
        return Failure("Setup is only available before users exist");
      }

      const configuredAdminEmail = this.getConfiguredAdminEmail();
      if (
        configuredAdminEmail &&
        credentials.email.trim() !== configuredAdminEmail
      ) {
        return Failure("Setup email does not match configured admin email");
      }

      const lockResult = await this.setupLocks.withLock(async () => {
        const usersBeforeInsert = await this.users.count();
        if (usersBeforeInsert > 0) {
          return Failure<{ token: string; user: AuthUser }, string>(
            "Setup is only available before users exist",
          );
        }

        const passwordHash = await hashPassword(credentials.password);
        const user = await this.users.createFirstAdmin(
          {
            username: credentials.username,
            email: credentials.email,
            firstName: credentials.firstName,
            lastName: credentials.lastName,
          },
          passwordHash,
        );

        return {
          success: true,
          error: null,
          data: this.signUser(user),
        };
      });

      if (!lockResult.acquired) {
        return Failure("Setup is already in progress");
      }

      return lockResult.value;
    } catch (error) {
      if (isConnectionError(error)) {
        throw new ServiceUnavailableException(
          "Database temporarily unavailable",
        );
      }
      return Failure("Setup failed");
    }
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
