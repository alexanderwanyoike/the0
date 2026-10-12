import { Module } from "@nestjs/common";
import { UserController } from "./user.controller";
import { AdminBootstrapService } from "./admin-bootstrap.service";
import { AdminMutationLockRepository } from "./admin-mutation-lock.repository";
import { UserRepository } from "./user.repository";
import { UserService } from "./user.service";

@Module({
  controllers: [UserController],
  providers: [
    UserService,
    UserRepository,
    AdminMutationLockRepository,
    AdminBootstrapService,
  ],
  exports: [UserService],
})
export class UserModule {}
