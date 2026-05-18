import { Request } from "express";
import { UserRole } from "@/user/user.constants";

export interface AuthUser {
  id: string;
  username: string;
  email: string;
  firstName?: string;
  lastName?: string;
  isActive: boolean;
  isEmailVerified: boolean;
  role: UserRole;
  isConfiguredRootAdmin?: boolean;
}

/** User payload attached to the request by auth guards */
export interface AuthenticatedUser {
  uid: string;
  id: string;
  username: string;
  email: string;
  firstName: string | null;
  lastName: string | null;
  isActive: boolean;
  isEmailVerified: boolean;
  role: "admin" | "user";
  authType: "jwt" | "apikey";
}

/** Express request with authenticated user attached */
export interface AuthenticatedRequest extends Request {
  user: AuthenticatedUser;
}
