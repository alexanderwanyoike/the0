import { Module } from "@nestjs/common";
import { ApiKeyController } from "./api-key.controller";
import { ApiKeyService } from "./api-key.service";
import { ApiKeyRepository } from "./api-key.repository";
import { UserRepository } from "@/user/user.repository";

@Module({
  controllers: [ApiKeyController],
  providers: [ApiKeyService, ApiKeyRepository, UserRepository],
  exports: [ApiKeyService, ApiKeyRepository],
})
export class ApiKeyModule {}
