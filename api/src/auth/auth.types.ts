import { Request } from "express";

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
  authType: "jwt" | "apikey";
}

/** Express request with authenticated user attached */
export interface AuthenticatedRequest extends Request {
  user: AuthenticatedUser;
}
