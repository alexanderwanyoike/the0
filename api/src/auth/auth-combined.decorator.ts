import { SetMetadata } from "@nestjs/common";

export const COMBINED_AUTH_KEY = "combinedAuth";
export const CombinedAuth = () => SetMetadata(COMBINED_AUTH_KEY, true);

// src/auth/decorators/auth-type.decorator.ts
import { createParamDecorator, ExecutionContext } from "@nestjs/common";

export const AuthType = createParamDecorator(
  (data: unknown, ctx: ExecutionContext) => {
    const request = ctx.switchToHttp().getRequest();
    return request.user?.authType || "unknown";
  },
);
