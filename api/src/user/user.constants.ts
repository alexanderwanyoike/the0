export const USER_ROLES = {
  ADMIN: "admin",
  USER: "user",
} as const;

export const USER_ROLE_VALUES = [USER_ROLES.ADMIN, USER_ROLES.USER] as const;

export type UserRole = (typeof USER_ROLE_VALUES)[number];
