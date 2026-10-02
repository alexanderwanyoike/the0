export interface AuthUser {
  id: string;
  username: string;
  email: string;
  firstName?: string;
  lastName?: string;
  isActive: boolean;
  isEmailVerified: boolean;
  role: "admin" | "user";
  isConfiguredRootAdmin?: boolean;
}

export interface LoginCredentials {
  email: string;
  password: string;
}

export interface AuthResponse {
  token: string;
  user: AuthUser;
}

export interface AuthApiResponse<T> {
  success: boolean;
  data?: T;
  message?: string;
}

export interface Result<T, E = string> {
  success: boolean;
  data?: T;
  error?: E;
}
