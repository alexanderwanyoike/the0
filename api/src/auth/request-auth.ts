import { Request } from "express";
import { AuthenticatedUser, AuthUser } from "./auth.types";

export function extractAuthHeaderToken(
  request: Request,
  scheme: "Bearer" | "ApiKey",
): string | null {
  const authHeader = request.headers.authorization;
  if (!authHeader) {
    return null;
  }

  const [type, token] = authHeader.split(" ");
  return type === scheme && token ? token : null;
}

export function toAuthenticatedJwtUser(user: AuthUser): AuthenticatedUser {
  return {
    uid: user.id,
    id: user.id,
    username: user.username,
    email: user.email,
    firstName: user.firstName || null,
    lastName: user.lastName || null,
    isActive: user.isActive,
    isEmailVerified: user.isEmailVerified,
    role: user.role,
    authType: "jwt",
  };
}
