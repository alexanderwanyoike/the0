"use client";
import React, { createContext, useContext, useEffect, useState } from "react";
import { useRouter } from "next/navigation";
import { JwtAuthService } from "@/lib/auth/jwt-auth.service";
import { AuthUser } from "@/lib/auth/types";
import { setAuthService } from "@/lib/axios-interceptor";
import { setAuthFetchService } from "@/lib/auth-fetch";
import { setApiClientService } from "@/lib/api-client";
import { setSSEAuthService } from "@/lib/sse/sse-auth";

interface AuthContextType {
  user: AuthUser | null;
  userData: AuthUser | null; // Keep userData for backward compatibility, maps to user
  loading: boolean;
  login: (credentials: {
    email: string;
    password: string;
  }) => Promise<{ success: boolean; error?: string }>;
  register: (credentials: {
    username: string;
    email: string;
    password: string;
  }) => Promise<{ success: boolean; error?: string }>;
  logout: () => Promise<void>;
  token: string | null;
  authService: JwtAuthService; // Expose the centralized auth service
}

const AuthContext = createContext<AuthContextType>({
  user: null,
  userData: null,
  loading: true,
  login: async () => ({ success: false, error: "Not initialized" }),
  register: async () => ({ success: false, error: "Not initialized" }),
  logout: async () => {},
  token: null,
  authService: new JwtAuthService(),
});

export function AuthProvider({ children }: { children: React.ReactNode }) {
  const [user, setUser] = useState<AuthUser | null>(null);
  const [token, setToken] = useState<string | null>(null);
  const [loading, setLoading] = useState(true);
  const router = useRouter();

  // Centralized auth service instance
  const authService = new JwtAuthService();

  const initializeAuth = async () => {
    setLoading(true);
    try {
      const storedToken = authService.getToken();
      if (storedToken) {
        const isValid = await authService.validateToken(storedToken);
        if (isValid.success) {
          const currentUser = await authService.getCurrentUser();
          if (currentUser.success && currentUser.data) {
            setUser(currentUser.data);
            setToken(storedToken);
            // Set cookie for server-side auth
            document.cookie = `auth-token=${storedToken}; path=/`;
          } else {
            // Token invalid, clear it
            authService.logout();
            setUser(null);
            setToken(null);
            document.cookie =
              "auth-token=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT";
          }
        } else {
          // Token invalid, clear it
          authService.logout();
          setUser(null);
          setToken(null);
          document.cookie =
            "auth-token=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT";
        }
      } else {
        setUser(null);
        setToken(null);
      }
    } catch (error) {
      console.error("Error initializing auth:", error);
      setUser(null);
      setToken(null);
    } finally {
      setLoading(false);
    }
  };

  // Initialize auth state on mount and register with all auth utilities
  useEffect(() => {
    // Register auth service with all API utilities
    setAuthService(authService, logout);
    setAuthFetchService(authService, logout);
    setApiClientService(authService, logout);
    setSSEAuthService(authService, logout);

    // Initialize auth state
    initializeAuth();
  }, []);

  const login = async (credentials: { email: string; password: string }) => {
    try {
      const result = await authService.login(credentials);
      if (result.success && result.data) {
        setUser(result.data.user);
        setToken(result.data.token);
        // Set cookie for server-side auth
        document.cookie = `auth-token=${result.data.token}; path=/`;
        router.push("/dashboard");
        return { success: true };
      } else {
        return { success: false, error: result.error || "Login failed" };
      }
    } catch (error) {
      console.error("Login error:", error);
      return { success: false, error: "Login failed. Please try again." };
    }
  };

  const register = async (credentials: {
    username: string;
    email: string;
    password: string;
  }) => {
    try {
      const result = await authService.register(credentials);
      if (result.success && result.data) {
        setUser(result.data.user);
        setToken(result.data.token);
        // Set cookie for server-side auth
        document.cookie = `auth-token=${result.data.token}; path=/`;
        router.push("/dashboard");
        return { success: true };
      } else {
        return { success: false, error: result.error || "Registration failed" };
      }
    } catch (error) {
      console.error("Registration error:", error);
      return {
        success: false,
        error: "Registration failed. Please try again.",
      };
    }
  };

  const logout = async () => {
    try {
      authService.logout();
      setUser(null);
      setToken(null);
      // Remove cookie when logged out
      document.cookie =
        "auth-token=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT";
      // Use proper React routing instead of window.location
      router.push("/login");
    } catch (error) {
      console.error("Error signing out:", error);
    }
  };

  return (
    <AuthContext.Provider
      value={{
        user,
        loading,
        login,
        register,
        logout,
        userData: user,
        token,
        authService,
      }}
    >
      {!loading && children}
    </AuthContext.Provider>
  );
}

export const useAuth = () => useContext(AuthContext);
