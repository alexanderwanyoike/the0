import { Injectable, Logger, OnModuleInit } from "@nestjs/common";
import { USER_ROLES } from "@/user/user.constants";
import { UserRepository } from "@/user/user.repository";

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

  constructor(private readonly users: UserRepository) {}

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

  private async bootstrapAdminFromExistingUsers(): Promise<void> {
    const users = await this.users.list();

    if (users.length === 0) {
      return;
    }

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
      return;
    }

    const configuredAdminEmail = this.getConfiguredAdminEmail();
    if (configuredAdminEmail) {
      const matchingActiveUser = refreshedUsers.find(
        (user) => user.isActive === true && user.email === configuredAdminEmail,
      );

      if (matchingActiveUser) {
        await this.users.promoteToAdmin(matchingActiveUser.id);
        this.logger.log(
          `Promoted configured admin user ${configuredAdminEmail}`,
        );
        return;
      }
    }

    this.logger.warn(
      "No admin configured. Set THE0_ADMIN_EMAIL to an existing active user or see docs/deployment/admin-bootstrap.md",
    );
  }
}
