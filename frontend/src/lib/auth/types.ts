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

export interface SetupCredentials {
  username: string;
  email: string;
  password: string;
  firstName?: string;
  lastName?: string;
}

export interface AuthResponse {
  token: string;
  user: AuthUser;
}

export interface ApiResponse<T> {
  success: boolean;
  data?: T;
  message?: string;
}

export interface ApiError {
  message: string;
  statusCode?: number;
}

export interface Result<T, E = string> {
  success: boolean;
  data?: T;
  error?: E;
}
