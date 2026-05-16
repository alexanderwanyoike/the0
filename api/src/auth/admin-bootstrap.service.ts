import { Injectable, Logger, OnModuleInit } from "@nestjs/common";
import { AuthService } from "./auth.service";

@Injectable()
export class AdminBootstrapService implements OnModuleInit {
  private readonly logger = new Logger(AdminBootstrapService.name);

  constructor(private readonly authService: AuthService) {}

  async onModuleInit(): Promise<void> {
    try {
      await this.authService.bootstrapAdminFromExistingUsers();
    } catch (error) {
      const stack = error instanceof Error ? error.stack : undefined;
      this.logger.error("Admin bootstrap check failed", stack);
    }
  }
}
