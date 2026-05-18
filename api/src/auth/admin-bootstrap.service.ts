import { Injectable, Logger, OnModuleInit } from "@nestjs/common";
import * as bcrypt from "bcrypt";
import { hashPassword } from "@/common/password";
import { validatePasswordPolicy } from "@/common/password-policy";
import { AdminMutationLockRepository } from "@/user/admin-mutation-lock.repository";
import { USER_ROLES } from "@/user/user.constants";
import { UserRepository } from "@/user/user.repository";
import { UserRecord } from "@/user/user.types";
import { SetupLockRepository } from "./setup-lock.repository";

function metadataRole(metadata: unknown): string | undefined {
  if (!metadata || typeof metadata !== "object") {
    return undefined;
  }
  const role = (metadata as { role?: unknown }).role;
  return typeof role === "string" ? role : undefined;
}

@Injectable()
export class AdminBootstrapService implements OnModuleInit {
  private readonly logger = new Logger(AdminBootstrapService.name);

  constructor(
    private readonly users: UserRepository,
    private readonly setupLocks: SetupLockRepository,
    private readonly adminMutationLocks: AdminMutationLockRepository,
  ) {}

  async onModuleInit(): Promise<void> {
    try {
      await this.bootstrapAdminFromExistingUsers();
    } catch (error) {
      const stack = error instanceof Error ? error.stack : undefined;
      this.logger.error("Admin bootstrap check failed", stack);
    }
  }

  private getConfiguredAdminEmail(): string | undefined {
    const email = process.env.THE0_ADMIN_EMAIL?.trim();
    return email || undefined;
  }

  private getConfiguredAdminPassword(): string | undefined {
    const password = process.env.THE0_ADMIN_PASSWORD;
    return password === undefined || password === "" ? undefined : password;
  }

  private deriveUsername(email: string): string {
    return email.split("@")[0]?.trim() || email;
  }

  private validateConfiguredPassword(password: string): boolean {
    const error = validatePasswordPolicy(password);
    if (!error) {
      return true;
    }

    this.logger.warn(`THE0_ADMIN_PASSWORD is invalid: ${error}`);
    return false;
  }

  private async bootstrapAdminFromExistingUsers(): Promise<void> {
    const userCount = await this.users.count();
    if (userCount === 0) {
      await this.createConfiguredFirstAdmin();
      return;
    }

    await this.adminMutationLocks.withLock(async () => {
      await this.bootstrapExistingUsersWithAdminMutationLock();
    });
  }

  private async createConfiguredFirstAdmin(): Promise<void> {
    const configuredAdminEmail = this.getConfiguredAdminEmail();
    const configuredAdminPassword = this.getConfiguredAdminPassword();

    if (!configuredAdminEmail || !configuredAdminPassword) {
      return;
    }

    if (!this.validateConfiguredPassword(configuredAdminPassword)) {
      return;
    }

    const lockResult = await this.setupLocks.withLock(async () => {
      const usersBeforeInsert = await this.users.count();
      if (usersBeforeInsert > 0) {
        return;
      }

      const passwordHash = await hashPassword(configuredAdminPassword);
      await this.users.createFirstAdmin(
        {
          username: this.deriveUsername(configuredAdminEmail),
          email: configuredAdminEmail,
        },
        passwordHash,
      );

      this.logger.log("Created configured first admin user");
      this.warnRemoveConfiguredPassword();
    });

    if (!lockResult.acquired) {
      this.logger.warn("Configured admin creation skipped; setup is locked");
    }
  }

  private async bootstrapExistingUsersWithAdminMutationLock(): Promise<void> {
    const users = await this.users.list();

    for (const user of users) {
      const roleFromMetadata = metadataRole(user.metadata);
      if (
        roleFromMetadata === USER_ROLES.ADMIN &&
        user.role !== USER_ROLES.ADMIN
      ) {
        await this.users.promoteToAdmin(user.id);
      }
    }

    const refreshedUsers = await this.users.list();
    const activeAdmins = refreshedUsers.filter(
      (user) => user.isActive && user.role === USER_ROLES.ADMIN,
    );
    if (activeAdmins.length > 0) {
      await this.applyConfiguredPasswordToExistingAdmin(refreshedUsers);
      return;
    }

    const configuredAdminEmail = this.getConfiguredAdminEmail();
    const configuredAdminPassword = this.getConfiguredAdminPassword();
    if (configuredAdminEmail && !configuredAdminPassword) {
      this.logger.warn(
        "No admin configured. Set THE0_ADMIN_PASSWORD with THE0_ADMIN_EMAIL to promote the configured active user",
      );
      return;
    }

    if (configuredAdminEmail && configuredAdminPassword) {
      if (!this.validateConfiguredPassword(configuredAdminPassword)) {
        return;
      }
      await this.promoteConfiguredActiveUser(
        refreshedUsers,
        configuredAdminEmail,
        configuredAdminPassword,
      );
      return;
    }

    const reason =
      "No admin configured. Set THE0_ADMIN_EMAIL and THE0_ADMIN_PASSWORD for an existing active user or see docs/deployment/admin-bootstrap.md";
    this.logger.warn(reason);
  }

  private async promoteConfiguredActiveUser(
    users: UserRecord[],
    configuredAdminEmail: string,
    configuredAdminPassword: string,
  ): Promise<void> {
    const matchingActiveUser = users.find(
      (user) => user.isActive === true && user.email === configuredAdminEmail,
    );

    if (!matchingActiveUser) {
      this.logger.warn(
        "Configured admin email does not match an active user; no admin was promoted",
      );
      return;
    }

    const passwordHash = await hashPassword(configuredAdminPassword);
    await this.users.promoteToAdminAndSetPassword(
      matchingActiveUser.id,
      passwordHash,
    );
    this.logger.log("Promoted configured admin user and set password");
    this.warnRemoveConfiguredPassword();
  }

  private async applyConfiguredPasswordToExistingAdmin(
    users: UserRecord[],
  ): Promise<void> {
    const configuredAdminEmail = this.getConfiguredAdminEmail();
    const configuredAdminPassword = this.getConfiguredAdminPassword();

    if (!configuredAdminEmail) {
      return;
    }

    const configuredUser = users.find(
      (user) => user.email === configuredAdminEmail,
    );
    if (
      !configuredUser ||
      !configuredUser.isActive ||
      configuredUser.role !== USER_ROLES.ADMIN
    ) {
      this.logger.warn(
        "Configured admin email does not match an active admin; skipping password update",
      );
      return;
    }

    if (!configuredAdminPassword) {
      return;
    }

    if (!this.validateConfiguredPassword(configuredAdminPassword)) {
      return;
    }

    if (!configuredUser.passwordHash) {
      const passwordHash = await hashPassword(configuredAdminPassword);
      await this.users.updatePassword(configuredUser.id, passwordHash);
      this.logger.log("Set configured admin password");
      this.warnRemoveConfiguredPassword();
      return;
    }

    const passwordMatches = await bcrypt.compare(
      configuredAdminPassword,
      configuredUser.passwordHash,
    );
    if (passwordMatches) {
      this.warnRemoveConfiguredPassword();
      return;
    }

    const passwordHash = await hashPassword(configuredAdminPassword);
    await this.users.updatePassword(configuredUser.id, passwordHash);
    this.logger.log("Updated configured admin password");
    this.warnRemoveConfiguredPassword();
  }

  private warnRemoveConfiguredPassword(): void {
    this.logger.warn(
      "THE0_ADMIN_PASSWORD is configured. Remove it after the admin password has been applied.",
    );
  }
}
