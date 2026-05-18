import { normalizeEmailForComparison } from "@/common/email";
import { USER_ROLES } from "@/user/user.constants";
import { UserRecord } from "@/user/user.types";
import { AuthUser } from "./auth.types";

function isConfiguredRootAdmin(user: UserRecord): boolean {
  const configuredEmail = process.env.THE0_ADMIN_EMAIL?.trim();
  return Boolean(
    configuredEmail &&
      normalizeEmailForComparison(user.email) ===
        normalizeEmailForComparison(configuredEmail),
  );
}

export function toAuthUser(user: UserRecord): AuthUser {
  return {
    id: user.id,
    username: user.username,
    email: user.email,
    firstName: user.firstName ?? undefined,
    lastName: user.lastName ?? undefined,
    isActive: Boolean(user.isActive),
    isEmailVerified: Boolean(user.isEmailVerified),
    role: user.role === USER_ROLES.ADMIN ? USER_ROLES.ADMIN : USER_ROLES.USER,
    isConfiguredRootAdmin: isConfiguredRootAdmin(user),
  };
}
