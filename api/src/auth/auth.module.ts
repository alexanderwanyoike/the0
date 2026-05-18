import { Global, Module } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { JwtModule } from "@nestjs/jwt";
import { PassportModule } from "@nestjs/passport";
import type { SignOptions } from "jsonwebtoken";
import { AuthController } from "./auth.controller";
import { AdminBootstrapService } from "./admin-bootstrap.service";
import { AdminJwtGuard } from "./admin-jwt.guard";
import { AuthService } from "./auth.service";
import { JwtAuthGuard } from "./jwt-auth.guard";
import { JwtStrategy } from "./jwt.strategy";
import { RootAdminCreationLockRepository } from "./root-admin-creation-lock.repository";
import { ApiKeyModule } from "@/api-key/api-key.module";
import { UserModule } from "@/user/user.module";

@Global()
@Module({
  imports: [
    PassportModule.register({ defaultStrategy: "jwt" }),
    JwtModule.registerAsync({
      inject: [ConfigService],
      useFactory: (configService: ConfigService) => ({
        secret: configService.getOrThrow<string>("JWT_SECRET"),
        signOptions: {
          expiresIn: (configService.get<string>("JWT_EXPIRES_IN") ||
            "24h") as SignOptions["expiresIn"],
          issuer: "the0-oss-api",
          audience: "the0-oss-clients",
        },
      }),
    }),
    ApiKeyModule,
    UserModule,
  ],
  providers: [
    JwtStrategy,
    AuthService,
    AdminBootstrapService,
    RootAdminCreationLockRepository,
    AdminJwtGuard,
    JwtAuthGuard,
  ],
  exports: [
    PassportModule,
    JwtModule,
    AuthService,
    AdminJwtGuard,
    JwtAuthGuard,
  ],
  controllers: [AuthController],
})
export class AuthModule {}
