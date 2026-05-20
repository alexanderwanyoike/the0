"use client";
import React, {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useState,
} from "react";
import { useRouter } from "next/navigation";
import { JwtAuthService } from "@/lib/auth/jwt-auth.service";
import { AuthUser } from "@/lib/auth/types";
import { setAuthService } from "@/lib/axios-interceptor";
import { setAuthFetchService } from "@/lib/auth-fetch";
import { setApiClientService } from "@/lib/api-client";
import { setSSEAuthService } from "@/lib/sse/sse-auth";

interface AuthContextType {
  user: AuthUser | null;
  userData: AuthUser | null;
  loading: boolean;
  login: (credentials: {
    email: string;
    password: string;
  }) => Promise<{ success: boolean; error?: string }>;
  logout: () => Promise<void>;
  refreshUser: () => Promise<void>;
  token: string | null;
  authService: JwtAuthService;
}

const AuthContext = createContext<AuthContextType>({
  user: null,
  userData: null,
  loading: true,
  login: async () => ({ success: false, error: "Not initialized" }),
  logout: async () => {},
  refreshUser: async () => {},
  token: null,
  authService: new JwtAuthService(),
});

export function AuthProvider({ children }: { children: React.ReactNode }) {
  const [user, setUser] = useState<AuthUser | null>(null);
  const [token, setToken] = useState<string | null>(null);
  const [loading, setLoading] = useState(true);
  const router = useRouter();
  const authService = useMemo(() => new JwtAuthService(), []);

  const clearAuthState = useCallback(() => {
    authService.logout();
    setUser(null);
    setToken(null);
    document.cookie =
      "auth-token=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT";
  }, [authService]);

  const initializeAuth = useCallback(async () => {
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
            document.cookie = `auth-token=${storedToken}; path=/`;
          } else {
            clearAuthState();
          }
        } else {
          clearAuthState();
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
  }, [authService, clearAuthState]);

  const logout = useCallback(async () => {
    try {
      clearAuthState();
      router.push("/login");
    } catch (error) {
      console.error("Error signing out:", error);
    }
  }, [clearAuthState, router]);

  useEffect(() => {
    setAuthService(authService, logout);
    setAuthFetchService(authService, logout);
    setApiClientService(authService, logout);
    setSSEAuthService(authService, logout);

    initializeAuth();
  }, [authService, initializeAuth, logout]);

  const login = async (credentials: { email: string; password: string }) => {
    try {
      const result = await authService.login(credentials);
      if (result.success && result.data) {
        setUser(result.data.user);
        setToken(result.data.token);
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

  const refreshUser = async () => {
    try {
      const currentUser = await authService.getCurrentUser();
      if (currentUser.success && currentUser.data) {
        setUser(currentUser.data);
        return;
      }
      await logout();
    } catch (error) {
      console.error("Error refreshing user:", error);
      await logout();
    }
  };

  return (
    <AuthContext.Provider
      value={{
        user,
        loading,
        login,
        logout,
        refreshUser,
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
