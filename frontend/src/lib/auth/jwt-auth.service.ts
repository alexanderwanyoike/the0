import {
  AuthUser,
  LoginCredentials,
  RegisterCredentials,
  AuthResponse,
  ApiResponse,
  Result,
} from "./types";

export class JwtAuthService {
  private readonly API_BASE_URL =
    process.env.NEXT_PUBLIC_API_URL || "http://localhost:3000";
  private readonly TOKEN_KEY = "auth-token";

  /**
   * Login with email and password
   */
  async login(credentials: LoginCredentials): Promise<Result<AuthResponse>> {
    try {
      const response = await fetch(`${this.API_BASE_URL}/auth/login`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(credentials),
      });

      const data: ApiResponse<AuthResponse> = await response.json();

      if (!response.ok) {
        return {
          success: false,
          error: data.message || "Login failed",
        };
      }

      if (data.success && data.data) {
        // Store token in localStorage
        this.setToken(data.data.token);

        return {
          success: true,
          data: data.data,
        };
      }

      return {
        success: false,
        error: "Invalid response format",
      };
    } catch (error) {
      console.error("Login error:", error);
      return {
        success: false,
        error: "Login failed. Please try again.",
      };
    }
  }

  /**
   * Register new user
   */
  async register(
    credentials: RegisterCredentials,
  ): Promise<Result<AuthResponse>> {
    try {
      const response = await fetch(`${this.API_BASE_URL}/auth/register`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(credentials),
      });

      const data: ApiResponse<AuthResponse> = await response.json();

      if (!response.ok) {
        return {
          success: false,
          error: data.message || "Registration failed",
        };
      }

      if (data.success && data.data) {
        // Store token in localStorage
        this.setToken(data.data.token);

        return {
          success: true,
          data: data.data,
        };
      }

      return {
        success: false,
        error: "Invalid response format",
      };
    } catch (error) {
      console.error("Registration error:", error);
      return {
        success: false,
        error: "Registration failed. Please try again.",
      };
    }
  }

  /**
   * Validate JWT token
   */
  async validateToken(token: string): Promise<Result<AuthUser>> {
    try {
      const response = await fetch(`${this.API_BASE_URL}/auth/validate`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({ token }),
      });

      const data: ApiResponse<AuthUser> = await response.json();

      if (!response.ok) {
        return {
          success: false,
          error: data.message || "Token validation failed",
        };
      }

      if (data.success && data.data) {
        return {
          success: true,
          data: data.data,
        };
      }

      return {
        success: false,
        error: "Invalid response format",
      };
    } catch (error) {
      console.error("Token validation error:", error);
      return {
        success: false,
        error: "Token validation failed",
      };
    }
  }

  /**
   * Get current user from token
   */
  async getCurrentUser(): Promise<Result<AuthUser>> {
    const token = this.getToken();

    if (!token) {
      return {
        success: false,
        error: "No authentication token found",
      };
    }

    try {
      const response = await fetch(`${this.API_BASE_URL}/auth/me`, {
        method: "GET",
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });

      const data: ApiResponse<AuthUser> = await response.json();

      if (!response.ok) {
        // If token is invalid, remove it
        if (response.status === 401) {
          this.removeToken();
        }

        return {
          success: false,
          error: data.message || "Failed to get current user",
        };
      }

      if (data.success && data.data) {
        return {
          success: true,
          data: data.data,
        };
      }

      return {
        success: false,
        error: "Invalid response format",
      };
    } catch (error) {
      console.error("Get current user error:", error);
      return {
        success: false,
        error: "Failed to get current user",
      };
    }
  }

  /**
   * Logout user
   */
  logout(): void {
    this.removeToken();
  }

  /**
   * Get stored token
   */
  getToken(): string | null {
    if (typeof window === "undefined") return null;
    return localStorage.getItem(this.TOKEN_KEY);
  }

  /**
   * Check if user is authenticated
   */
  isAuthenticated(): boolean {
    return this.getToken() !== null;
  }

  /**
   * Store token in localStorage
   */
  private setToken(token: string): void {
    if (typeof window !== "undefined") {
      localStorage.setItem(this.TOKEN_KEY, token);
    }
  }

  /**
   * Remove token from localStorage
   */
  private removeToken(): void {
    if (typeof window !== "undefined") {
      localStorage.removeItem(this.TOKEN_KEY);
    }
  }
}
