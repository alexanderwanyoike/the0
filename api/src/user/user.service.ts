import {
  BadRequestException,
  ForbiddenException,
  Injectable,
  NotFoundException,
  UnauthorizedException,
} from "@nestjs/common";
import * as bcrypt from "bcrypt";
import { hashPassword } from "@/common/password";
import { AuthenticatedUser } from "@/auth/auth.types";
import { AdminMutationLockRepository } from "./admin-mutation-lock.repository";
import { USER_ROLES, UserRole } from "./user.constants";
import { UserRepository } from "./user.repository";
import {
  CreateAdminUserInput,
  SerializedUser,
  UpdateAdminUserInput,
  UpdateProfileInput,
  UserRecord,
} from "./user.types";

@Injectable()
export class UserService {
  constructor(
    private readonly users: UserRepository,
    private readonly adminMutationLock: AdminMutationLockRepository,
  ) {}

  private serializeUser(user: UserRecord): SerializedUser {
    return {
      id: user.id,
      username: user.username,
      email: user.email,
      firstName: user.firstName,
      lastName: user.lastName,
      role: user.role === USER_ROLES.ADMIN ? USER_ROLES.ADMIN : USER_ROLES.USER,
      isActive: Boolean(user.isActive),
      isEmailVerified: Boolean(user.isEmailVerified),
      isConfiguredRootAdmin: this.isConfiguredRootAdmin(user),
      lastLoginAt: user.lastLoginAt,
      createdAt: user.createdAt,
      updatedAt: user.updatedAt,
    };
  }

  private defaultUsername(email: string): string {
    return email.split("@")[0] || email;
  }

  private normalizeUsername(username: string | undefined, email: string) {
    const candidate = username?.trim() || this.defaultUsername(email);
    return candidate.trim();
  }

  private getConfiguredRootAdminEmail(): string | undefined {
    const email = process.env.THE0_ADMIN_EMAIL?.trim();
    return email || undefined;
  }

  private isConfiguredRootAdmin(user: Pick<UserRecord, "email">): boolean {
    const configuredEmail = this.getConfiguredRootAdminEmail();
    return Boolean(configuredEmail && user.email === configuredEmail);
  }

  private assertRootAdminEmailUnchanged(
    user: UserRecord,
    nextEmail: string | undefined,
  ): void {
    if (
      nextEmail !== undefined &&
      nextEmail !== user.email &&
      this.isConfiguredRootAdmin(user)
    ) {
      throw new ForbiddenException(
        "Configured root admin email is managed by deployment configuration",
      );
    }
  }

  private assertRootAdminPasswordUnchanged(user: UserRecord): void {
    if (this.isConfiguredRootAdmin(user)) {
      throw new ForbiddenException(
        "Configured root admin password is managed by deployment configuration",
      );
    }
  }

  async listUsers() {
    const users = await this.users.list();
    return users.map((user) => this.serializeUser(user));
  }

  async createUser(input: CreateAdminUserInput) {
    const email = input.email.trim();
    const username = this.normalizeUsername(input.username, email);

    const existingByEmail = await this.users.findByEmail(email);
    if (existingByEmail) {
      throw new BadRequestException("Email is already in use");
    }

    const existingByUsername = await this.users.findByUsername(username);
    if (existingByUsername) {
      throw new BadRequestException("Username is already in use");
    }

    const passwordHash = await hashPassword(input.password);
    const user = await this.users.createUser(
      input,
      username,
      email,
      passwordHash,
    );

    return this.serializeUser(user);
  }

  async updateUser(
    id: string,
    input: UpdateAdminUserInput,
    actor: AuthenticatedUser,
  ) {
    return this.adminMutationLock.withLock(() =>
      this.updateUserWithAdminMutationLock(id, input, actor),
    );
  }

  private async updateUserWithAdminMutationLock(
    id: string,
    input: UpdateAdminUserInput,
    actor: AuthenticatedUser,
  ) {
    const user = await this.requireUser(id);

    if (id === actor.id && input.role && input.role !== USER_ROLES.ADMIN) {
      throw new ForbiddenException("You cannot demote your own admin account");
    }
    if (id === actor.id && input.isActive === false) {
      throw new ForbiddenException(
        "You cannot deactivate your own admin account",
      );
    }

    await this.assertAdminWillRemain(user, input);

    const nextEmail = input.email?.trim();
    const nextUsername = input.username?.trim();
    this.assertRootAdminEmailUnchanged(user, nextEmail);

    if (nextEmail && nextEmail !== user.email) {
      const existing = await this.users.findByEmail(nextEmail);
      if (existing) {
        throw new BadRequestException("Email is already in use");
      }
    }

    if (nextUsername && nextUsername !== user.username) {
      const existing = await this.users.findByUsername(nextUsername);
      if (existing) {
        throw new BadRequestException("Username is already in use");
      }
    }

    const updates: UpdateAdminUserInput = {};
    if (nextUsername !== undefined) {
      updates.username = nextUsername;
    }
    if (nextEmail !== undefined) {
      updates.email = nextEmail;
    }
    if (input.firstName !== undefined) {
      updates.firstName = input.firstName;
    }
    if (input.lastName !== undefined) {
      updates.lastName = input.lastName;
    }
    if (input.role !== undefined) {
      updates.role = input.role;
    }
    if (input.isActive !== undefined) {
      updates.isActive = input.isActive;
    }

    const updated = await this.users.update(id, updates);

    return this.serializeUser(updated);
  }

  async resetPassword(id: string, password: string, actor?: AuthenticatedUser) {
    if (actor?.id === id) {
      throw new ForbiddenException(
        "Use change password for your own admin account",
      );
    }

    const user = await this.requireUser(id);
    this.assertRootAdminPasswordUnchanged(user);

    const passwordHash = await hashPassword(password);
    const updated = await this.users.updatePassword(id, passwordHash);

    return this.serializeUser(updated);
  }

  async updateProfile(actor: AuthenticatedUser, input: UpdateProfileInput) {
    const updates: UpdateProfileInput = {};

    const nextUsername = input.username?.trim();

    if (nextUsername) {
      const existing = await this.users.findAnotherByUsername(
        nextUsername,
        actor.id,
      );
      if (existing) {
        throw new BadRequestException("Username is already in use");
      }
      updates.username = nextUsername;
    }
    if (input.firstName !== undefined) updates.firstName = input.firstName;
    if (input.lastName !== undefined) updates.lastName = input.lastName;

    const updated = await this.users.updateProfile(actor.id, updates);

    return this.serializeUser(updated);
  }

  async changePassword(
    actor: AuthenticatedUser,
    currentPassword: string,
    newPassword: string,
  ) {
    const user = await this.requireUser(actor.id);
    this.assertRootAdminPasswordUnchanged(user);

    const passwordMatches = await bcrypt.compare(
      currentPassword,
      user.passwordHash,
    );
    if (!passwordMatches) {
      throw new UnauthorizedException("Current password is incorrect");
    }

    const passwordHash = await hashPassword(newPassword);
    await this.users.updatePassword(actor.id, passwordHash);

    return { success: true, requiresLogin: true };
  }

  async deleteAccount(actor: AuthenticatedUser, password: string) {
    return this.adminMutationLock.withLock(() =>
      this.deleteAccountWithAdminMutationLock(actor, password),
    );
  }

  private async deleteAccountWithAdminMutationLock(
    actor: AuthenticatedUser,
    password: string,
  ) {
    const user = await this.requireUser(actor.id);

    const passwordMatches = await bcrypt.compare(password, user.passwordHash);
    if (!passwordMatches) {
      throw new UnauthorizedException("Password is incorrect");
    }

    await this.assertAdminWillRemain(user, { isActive: false });

    await this.users.deactivate(actor.id);
    return { success: true };
  }

  private async requireUser(id: string): Promise<UserRecord> {
    const user = await this.users.findById(id);
    if (!user) {
      throw new NotFoundException("User not found");
    }
    return user;
  }

  private async assertAdminWillRemain(
    currentUser: UserRecord,
    input: { role?: UserRole; isActive?: boolean },
  ) {
    if (currentUser.role !== USER_ROLES.ADMIN || !currentUser.isActive) {
      return;
    }

    const wouldRemoveAdmin =
      input.role === USER_ROLES.USER || input.isActive === false;
    if (!wouldRemoveAdmin) {
      return;
    }

    const activeAdminCount = await this.users.countActiveAdmins();

    if (activeAdminCount <= 1) {
      throw new ForbiddenException("At least one active admin is required");
    }
  }
}
