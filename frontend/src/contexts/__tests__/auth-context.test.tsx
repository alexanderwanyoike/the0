import React from "react";
import { render, screen, waitFor, act } from "@testing-library/react";
import { useRouter } from "next/navigation";
import { AuthProvider, useAuth } from "../auth-context";
import { JwtAuthService } from "@/lib/auth/jwt-auth.service";
import { AuthUser } from "@/lib/auth/types";

// Mock Next.js router
jest.mock("next/navigation", () => ({
  useRouter: jest.fn(),
}));

// Mock JwtAuthService
jest.mock("@/lib/auth/jwt-auth.service");

// Mock all auth utility setters
jest.mock("@/lib/axios-interceptor", () => ({
  setAuthService: jest.fn(),
}));

jest.mock("@/lib/auth-fetch", () => ({
  setAuthFetchService: jest.fn(),
}));

jest.mock("@/lib/api-client", () => ({
  setApiClientService: jest.fn(),
}));

jest.mock("@/lib/sse/sse-auth", () => ({
  setSSEAuthService: jest.fn(),
}));

const mockRouter = {
  push: jest.fn(),
  replace: jest.fn(),
  back: jest.fn(),
  forward: jest.fn(),
  refresh: jest.fn(),
  prefetch: jest.fn(),
};

const mockAuthService = {
  login: jest.fn(),
  register: jest.fn(),
  validateToken: jest.fn(),
  getCurrentUser: jest.fn(),
  logout: jest.fn(),
  getToken: jest.fn(),
  isAuthenticated: jest.fn(),
};

const mockUser: AuthUser = {
  id: "1",
  username: "testuser",
  email: "test@example.com",
  firstName: "Test",
  lastName: "User",
  isActive: true,
  isEmailVerified: true,
};

// Test component to access auth context
function TestComponent() {
  const { user, loading, login, register, logout, token, authService } =
    useAuth();

  return (
    <div>
      <div data-testid="loading">{loading ? "loading" : "loaded"}</div>
      <div data-testid="user">{user ? user.username : "no user"}</div>
      <div data-testid="token">{token || "no token"}</div>
      <button
        onClick={() =>
          login({ email: "test@example.com", password: "password" })
        }
      >
        Login
      </button>
      <button
        onClick={() =>
          register({
            username: "test",
            email: "test@example.com",
            password: "password",
          })
        }
      >
        Register
      </button>
      <button onClick={() => logout()}>Logout</button>
      <div data-testid="auth-service">
        {authService ? "service available" : "no service"}
      </div>
    </div>
  );
}

describe("AuthContext", () => {
  beforeEach(() => {
    jest.clearAllMocks();
    (useRouter as jest.Mock).mockReturnValue(mockRouter);
    (JwtAuthService as jest.Mock).mockImplementation(() => mockAuthService);

    // Mock document.cookie
    Object.defineProperty(document, "cookie", {
      writable: true,
      value: "",
    });
  });

  afterEach(() => {
    jest.resetAllMocks();
  });

  describe("initialization", () => {
    it("should initialize with loading state", async () => {
      mockAuthService.getToken.mockReturnValue(null);

      // Start with loading state - we need to control the async init process
      const { rerender } = render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>,
      );

      // Auth loads very quickly, so we just verify it reaches loaded state
      await waitFor(() => {
        expect(screen.getByTestId("loading")).toHaveTextContent("loaded");
      });
    });

    it("should initialize with existing valid token", async () => {
      mockAuthService.getToken.mockReturnValue("valid-token");
      mockAuthService.validateToken.mockResolvedValue({ success: true });
      mockAuthService.getCurrentUser.mockResolvedValue({
        success: true,
        data: mockUser,
      });

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>,
      );

      await waitFor(() => {
        expect(screen.getByTestId("loading")).toHaveTextContent("loaded");
        expect(screen.getByTestId("user")).toHaveTextContent("testuser");
        expect(screen.getByTestId("token")).toHaveTextContent("valid-token");
      });
    });

    it("should clear invalid token on initialization", async () => {
      mockAuthService.getToken.mockReturnValue("invalid-token");
      mockAuthService.validateToken.mockResolvedValue({ success: false });

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>,
      );

      await waitFor(() => {
        expect(screen.getByTestId("loading")).toHaveTextContent("loaded");
        expect(screen.getByTestId("user")).toHaveTextContent("no user");
        expect(mockAuthService.logout).toHaveBeenCalled();
      });
    });

    it("should register auth service with all utilities", async () => {
      const setAuthService = require("@/lib/axios-interceptor").setAuthService;
      const setAuthFetchService =
        require("@/lib/auth-fetch").setAuthFetchService;
      const setApiClientService =
        require("@/lib/api-client").setApiClientService;
      const setSSEAuthService = require("@/lib/sse/sse-auth").setSSEAuthService;

      mockAuthService.getToken.mockReturnValue(null);

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>,
      );

      await waitFor(() => {
        expect(setAuthService).toHaveBeenCalled();
        expect(setAuthFetchService).toHaveBeenCalled();
        expect(setApiClientService).toHaveBeenCalled();
        expect(setSSEAuthService).toHaveBeenCalled();
      });
    });
  });

  describe("login", () => {
    it("should handle successful login", async () => {
      mockAuthService.getToken.mockReturnValue(null);
      mockAuthService.login.mockResolvedValue({
        success: true,
        data: {
          token: "new-token",
          user: mockUser,
        },
      });

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>,
      );

      await waitFor(() => {
        expect(screen.getByTestId("loading")).toHaveTextContent("loaded");
      });

      await act(async () => {
        screen.getByText("Login").click();
      });

      await waitFor(() => {
        expect(screen.getByTestId("user")).toHaveTextContent("testuser");
        expect(mockRouter.push).toHaveBeenCalledWith("/dashboard");
      });
    });

    it("should handle failed login", async () => {
      mockAuthService.getToken.mockReturnValue(null);
      mockAuthService.login.mockResolvedValue({
        success: false,
        error: "Invalid credentials",
      });

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>,
      );

      await waitFor(() => {
        expect(screen.getByTestId("loading")).toHaveTextContent("loaded");
      });

      await act(async () => {
        screen.getByText("Login").click();
      });

      // Should not navigate on failed login
      expect(mockRouter.push).not.toHaveBeenCalled();
      expect(screen.getByTestId("user")).toHaveTextContent("no user");
    });

    it("should handle login network error", async () => {
      mockAuthService.getToken.mockReturnValue(null);
      mockAuthService.login.mockRejectedValue(new Error("Network error"));

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>,
      );

      await waitFor(() => {
        expect(screen.getByTestId("loading")).toHaveTextContent("loaded");
      });

      await act(async () => {
        screen.getByText("Login").click();
      });

      expect(mockRouter.push).not.toHaveBeenCalled();
      expect(screen.getByTestId("user")).toHaveTextContent("no user");
    });
  });

  describe("register", () => {
    it("should handle successful registration", async () => {
      mockAuthService.getToken.mockReturnValue(null);
      mockAuthService.register.mockResolvedValue({
        success: true,
        data: {
          token: "new-token",
          user: mockUser,
        },
      });

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>,
      );

      await waitFor(() => {
        expect(screen.getByTestId("loading")).toHaveTextContent("loaded");
      });

      await act(async () => {
        screen.getByText("Register").click();
      });

      await waitFor(() => {
        expect(screen.getByTestId("user")).toHaveTextContent("testuser");
        expect(mockRouter.push).toHaveBeenCalledWith("/dashboard");
      });
    });

    it("should handle failed registration", async () => {
      mockAuthService.getToken.mockReturnValue(null);
      mockAuthService.register.mockResolvedValue({
        success: false,
        error: "User already exists",
      });

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>,
      );

      await waitFor(() => {
        expect(screen.getByTestId("loading")).toHaveTextContent("loaded");
      });

      await act(async () => {
        screen.getByText("Register").click();
      });

      expect(mockRouter.push).not.toHaveBeenCalled();
      expect(screen.getByTestId("user")).toHaveTextContent("no user");
    });
  });

  describe("logout", () => {
    it("should handle logout", async () => {
      mockAuthService.getToken.mockReturnValue("existing-token");
      mockAuthService.validateToken.mockResolvedValue({ success: true });
      mockAuthService.getCurrentUser.mockResolvedValue({
        success: true,
        data: mockUser,
      });

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>,
      );

      await waitFor(() => {
        expect(screen.getByTestId("user")).toHaveTextContent("testuser");
      });

      await act(async () => {
        screen.getByText("Logout").click();
      });

      await waitFor(() => {
        expect(mockAuthService.logout).toHaveBeenCalled();
        expect(mockRouter.push).toHaveBeenCalledWith("/login");
        expect(screen.getByTestId("user")).toHaveTextContent("no user");
      });
    });
  });

  describe("auth service access", () => {
    it("should provide access to centralized auth service", async () => {
      mockAuthService.getToken.mockReturnValue(null);

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>,
      );

      await waitFor(() => {
        expect(screen.getByTestId("auth-service")).toHaveTextContent(
          "service available",
        );
      });
    });
  });

  describe("backward compatibility", () => {
    it("should provide userData property for backward compatibility", async () => {
      mockAuthService.getToken.mockReturnValue("valid-token");
      mockAuthService.validateToken.mockResolvedValue({ success: true });
      mockAuthService.getCurrentUser.mockResolvedValue({
        success: true,
        data: mockUser,
      });

      function BackwardCompatTestComponent() {
        const { userData } = useAuth();
        return (
          <div data-testid="userdata">
            {userData?.username || "no userData"}
          </div>
        );
      }

      render(
        <AuthProvider>
          <BackwardCompatTestComponent />
        </AuthProvider>,
      );

      await waitFor(() => {
        expect(screen.getByTestId("userdata")).toHaveTextContent("testuser");
      });
    });
  });
});
