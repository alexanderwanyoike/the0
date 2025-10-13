import { Module } from "@nestjs/common";
import { ApiKeyController } from "./api-key.controller";
import { ApiKeyService } from "./api-key.service";
import { ApiKeyRepository } from "./api-key.repository";

@Module({
  controllers: [ApiKeyController],
  providers: [ApiKeyService, ApiKeyRepository],
  exports: [ApiKeyService, ApiKeyRepository],
})
export class ApiKeyModule {}
