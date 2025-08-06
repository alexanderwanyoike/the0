"use client";

export interface ApiResponse<T = any> {
  data?: T;
  error?: {
    message: string;
    code?: string;
    statusCode?: number;
  };
}

// Global auth service reference - will be set by auth context
let globalAuthService: any = null;
let globalLogout: (() => Promise<void>) | null = null;

// Function to set the auth service from the context
export const setApiClientService = (
  authService: any,
  logoutFn: () => Promise<void>,
) => {
  globalAuthService = authService;
  globalLogout = logoutFn;
};

export class ApiClient {
  private static async getAuthToken(): Promise<string | null> {
    if (!globalAuthService) {
      return null;
    }
    return globalAuthService.getToken();
  }

  private static async makeRequest<T>(
    url: string,
    options: RequestInit = {},
  ): Promise<ApiResponse<T>> {
    const token = await this.getAuthToken();

    if (!token) {
      return {
        error: {
          message: "Not authenticated",
          code: "NO_AUTH",
          statusCode: 401,
        },
      };
    }

    const requestOptions: RequestInit = {
      ...options,
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${token}`,
        ...options.headers,
      },
    };

    try {
      const response = await fetch(url, requestOptions);
      const data = await response.json();

      if (!response.ok) {
        // If 401, use centralized logout
        if (response.status === 401) {
          if (globalLogout) {
            await globalLogout();
          } else if (globalAuthService) {
            globalAuthService.logout();
          }
        }

        return {
          error: {
            message: data.message || data.error || "Request failed",
            code: data.code,
            statusCode: response.status,
          },
        };
      }

      return { data };
    } catch (error: any) {
      return {
        error: {
          message: error.message || "Network error",
          code: "NETWORK_ERROR",
          statusCode: 500,
        },
      };
    }
  }

  static async get<T>(url: string): Promise<ApiResponse<T>> {
    return this.makeRequest<T>(url, { method: "GET" });
  }

  static async post<T>(url: string, data?: any): Promise<ApiResponse<T>> {
    return this.makeRequest<T>(url, {
      method: "POST",
      body: data ? JSON.stringify(data) : undefined,
    });
  }

  static async put<T>(url: string, data?: any): Promise<ApiResponse<T>> {
    return this.makeRequest<T>(url, {
      method: "PUT",
      body: data ? JSON.stringify(data) : undefined,
    });
  }

  static async delete<T>(url: string): Promise<ApiResponse<T>> {
    return this.makeRequest<T>(url, { method: "DELETE" });
  }
}
