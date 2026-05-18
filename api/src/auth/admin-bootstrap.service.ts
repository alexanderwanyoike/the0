import { Injectable, Logger, OnModuleInit } from "@nestjs/common";
import * as bcrypt from "bcrypt";
import { normalizeEmailForComparison } from "@/common/email";
import { hashPassword } from "@/common/password";
import {
  ConfiguredRootAdmin,
  getRequiredConfiguredRootAdmin,
} from "@/common/root-admin";
import { AdminMutationLockRepository } from "@/user/admin-mutation-lock.repository";
import { USER_ROLES } from "@/user/user.constants";
import { UserRepository } from "@/user/user.repository";
import { UserRecord } from "@/user/user.types";

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
    private readonly adminMutationLocks: AdminMutationLockRepository,
  ) {}

  async onModuleInit(): Promise<void> {
    await this.bootstrapConfiguredRootAdmin();
  }

  private deriveUsername(email: string): string {
    return email.split("@")[0]?.trim() || email;
  }

  private uniqueUsername(email: string, users: UserRecord[]): string {
    const baseUsername = this.deriveUsername(email);
    const usernames = new Set(users.map((user) => user.username));
    if (!usernames.has(baseUsername)) {
      return baseUsername;
    }

    let suffix = 1;
    let candidate = `${baseUsername}-admin`;
    while (usernames.has(candidate)) {
      suffix += 1;
      candidate = `${baseUsername}-admin-${suffix}`;
    }
    return candidate;
  }

  private async bootstrapConfiguredRootAdmin(): Promise<void> {
    const configuredAdmin = getRequiredConfiguredRootAdmin();
    await this.adminMutationLocks.withLock(async () => {
      await this.syncConfiguredRootAdminWithLock(configuredAdmin);
    });
  }

  private async createConfiguredFirstAdmin(
    configuredAdmin: ConfiguredRootAdmin,
  ): Promise<void> {
    const passwordHash = await hashPassword(configuredAdmin.password);
    await this.users.createFirstAdmin(
      {
        username: this.deriveUsername(configuredAdmin.email),
        email: configuredAdmin.email,
      },
      passwordHash,
    );
    this.logger.log("Created configured first admin user");
  }

  private async syncConfiguredRootAdminWithLock(
    configuredAdmin: ConfiguredRootAdmin,
  ): Promise<void> {
    const userCount = await this.users.count();
    if (userCount === 0) {
      await this.createConfiguredFirstAdmin(configuredAdmin);
      return;
    }

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
    const configuredUser = refreshedUsers.find(
      (user) =>
        normalizeEmailForComparison(user.email) === configuredAdmin.email,
    );

    if (!configuredUser) {
      await this.createConfiguredRootAdmin(refreshedUsers, configuredAdmin);
      return;
    }

    await this.syncConfiguredRootAdmin(
      configuredUser,
      configuredAdmin.password,
    );
  }

  private async createConfiguredRootAdmin(
    users: UserRecord[],
    configuredAdmin: ConfiguredRootAdmin,
  ): Promise<void> {
    const passwordHash = await hashPassword(configuredAdmin.password);
    await this.users.createUser(
      {
        email: configuredAdmin.email,
        password: "",
        role: USER_ROLES.ADMIN,
        isActive: true,
      },
      this.uniqueUsername(configuredAdmin.email, users),
      configuredAdmin.email,
      passwordHash,
    );
    this.logger.log("Created configured root admin user");
  }

  private async syncConfiguredRootAdmin(
    user: UserRecord,
    configuredPassword: string,
  ): Promise<void> {
    const passwordMatches =
      user.passwordHash &&
      (await bcrypt.compare(configuredPassword, user.passwordHash));

    if (!passwordMatches) {
      const passwordHash = await hashPassword(configuredPassword);
      await this.users.promoteToAdminAndSetPassword(user.id, passwordHash);
      this.logger.log("Synced configured root admin password");
      return;
    }

    if (!user.isActive || user.role !== USER_ROLES.ADMIN) {
      await this.users.update(user.id, {
        role: USER_ROLES.ADMIN,
        isActive: true,
      });
      this.logger.log("Synced configured root admin role");
    }
  }
}
