import { Module } from "@nestjs/common";
import { UserController } from "./user.controller";
import { AdminMutationLockRepository } from "./admin-mutation-lock.repository";
import { UserRepository } from "./user.repository";
import { UserService } from "./user.service";

@Module({
  controllers: [UserController],
  providers: [UserService, UserRepository, AdminMutationLockRepository],
  exports: [UserService, UserRepository],
})
export class UserModule {}
