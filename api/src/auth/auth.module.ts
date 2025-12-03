import { Global, Module } from "@nestjs/common";
import { JwtModule } from "@nestjs/jwt";
import { PassportModule } from "@nestjs/passport";
import { AuthController } from "./auth.controller";
import { AuthService } from "./auth.service";
import { JwtStrategy } from "./jwt.strategy";
import { ApiKeyModule } from "@/api-key/api-key.module";

@Global()
@Module({
  imports: [
    PassportModule.register({ defaultStrategy: "jwt" }),
    JwtModule.register({
      secret:
        process.env.JWT_SECRET || "the0-oss-jwt-secret-change-in-production",
      signOptions: {
        expiresIn: process.env.JWT_EXPIRES_IN || "24h",
        issuer: "the0-oss-api",
        audience: "the0-oss-clients",
      },
    }),
    ApiKeyModule,
  ],
  providers: [JwtStrategy, AuthService],
  exports: [PassportModule, JwtModule, AuthService],
  controllers: [AuthController],
})
export class AuthModule {}
