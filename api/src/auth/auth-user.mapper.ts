import { USER_ROLES } from "@/user/user.constants";
import { UserRecord } from "@/user/user.types";
import { AuthUser } from "./auth.types";

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
  };
}
