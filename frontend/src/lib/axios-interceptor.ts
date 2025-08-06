"use client";
import axios, { AxiosError, AxiosRequestConfig, AxiosResponse } from "axios";

// Create an axios instance with interceptors
const axiosInstance = axios.create();

// Global auth service reference - will be set by auth context
let globalAuthService: any = null;
let globalLogout: (() => Promise<void>) | null = null;

// Function to set the auth service from the context
export const setAuthService = (
  authService: any,
  logoutFn: () => Promise<void>,
) => {
  globalAuthService = authService;
  globalLogout = logoutFn;
};

// Request interceptor to add auth token
axiosInstance.interceptors.request.use(
  async (config) => {
    if (globalAuthService) {
      const token = globalAuthService.getToken();
      if (token && config.headers) {
        config.headers.Authorization = `Bearer ${token}`;
      }
    }
    return config;
  },
  (error) => {
    return Promise.reject(error);
  },
);

// Response interceptor to handle token refresh
axiosInstance.interceptors.response.use(
  (response: AxiosResponse) => {
    return response;
  },
  async (error: AxiosError) => {
    const originalRequest = error.config as AxiosRequestConfig & {
      _retry?: boolean;
    };

    if (error.response?.status === 401 && !originalRequest._retry) {
      // JWT token expired, logout user using centralized logout
      if (globalLogout) {
        await globalLogout();
      } else if (globalAuthService) {
        // Fallback to auth service logout if context logout not available
        globalAuthService.logout();
        if (typeof window !== "undefined") {
          document.cookie =
            "auth-token=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT";
          window.location.href = "/login";
        }
      }

      return Promise.reject(error);
    }

    return Promise.reject(error);
  },
);

export default axiosInstance;
