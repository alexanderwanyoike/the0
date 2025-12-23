import { UserService, ApiUser } from "../user.service";
import { ApiClient } from "@/lib/api-client";

// Mock ApiClient
jest.mock("@/lib/api-client", () => ({
  ApiClient: {
    get: jest.fn(),
    post: jest.fn(),
    put: jest.fn(),
    delete: jest.fn(),
  },
}));

const mockApiClient = ApiClient as jest.Mocked<typeof ApiClient>;

describe("UserService", () => {
  const mockUser: ApiUser = {
    id: "user-1",
    email: "test@example.com",
    displayName: "Test User",
    photoURL: null,
    createdAt: "2024-01-01T00:00:00.000Z",
    updatedAt: "2024-01-02T00:00:00.000Z",
    isActive: true,
    isEmailVerified: true,
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe("getUser", () => {
    it("should fetch user successfully", async () => {
      mockApiClient.get.mockResolvedValue({
        data: mockUser,
      });

      const result = await UserService.getUser("user-1");

      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.data?.id).toBe("user-1");
        expect(result.data?.email).toBe("test@example.com");
      }
      expect(mockApiClient.get).toHaveBeenCalledWith("/api/users/user-1");
    });

    it("should return null for non-existent user (404)", async () => {
      mockApiClient.get.mockResolvedValue({
        error: {
          message: "User not found",
          statusCode: 404,
        },
      });

      const result = await UserService.getUser("non-existent");

      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.data).toBeNull();
      }
    });

    it("should handle API error", async () => {
      mockApiClient.get.mockResolvedValue({
        error: {
          message: "Server error",
          statusCode: 500,
        },
      });

      const result = await UserService.getUser("user-1");

      expect(result.success).toBe(false);
      if (!result.success) {
        expect(result.error.status).toBe(500);
      }
    });

    it("should handle network error", async () => {
      mockApiClient.get.mockRejectedValue(new Error("Network error"));

      const result = await UserService.getUser("user-1");

      expect(result.success).toBe(false);
      if (!result.success) {
        expect(result.error.status).toBe(500);
      }
    });
  });

  describe("createUser", () => {
    it("should create user successfully", async () => {
      mockApiClient.post.mockResolvedValue({
        data: mockUser,
      });

      const result = await UserService.createUser({
        email: "test@example.com",
        displayName: "Test User",
        userId: "user-1",
      });

      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.data.email).toBe("test@example.com");
      }
      expect(mockApiClient.post).toHaveBeenCalledWith("/api/users", {
        email: "test@example.com",
        displayName: "Test User",
        userId: "user-1",
      });
    });

    it("should handle creation error", async () => {
      mockApiClient.post.mockResolvedValue({
        error: {
          message: "Email already exists",
          statusCode: 400,
        },
      });

      const result = await UserService.createUser({
        email: "existing@example.com",
        userId: "user-2",
      });

      expect(result.success).toBe(false);
      if (!result.success) {
        expect(result.error.error).toBe("Email already exists");
      }
    });
  });

  describe("updateUser", () => {
    it("should update user successfully", async () => {
      const updatedUser = { ...mockUser, displayName: "Updated Name" };
      mockApiClient.put.mockResolvedValue({
        data: updatedUser,
      });

      const result = await UserService.updateUser("user-1", {
        displayName: "Updated Name",
      });

      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.data.displayName).toBe("Updated Name");
      }
      expect(mockApiClient.put).toHaveBeenCalledWith("/api/users/user-1", {
        displayName: "Updated Name",
      });
    });

    it("should handle update error", async () => {
      mockApiClient.put.mockResolvedValue({
        error: {
          message: "User not found",
          statusCode: 404,
        },
      });

      const result = await UserService.updateUser("non-existent", {
        displayName: "Test",
      });

      expect(result.success).toBe(false);
    });
  });

  describe("updateLastLogin", () => {
    it("should update last login successfully", async () => {
      mockApiClient.put.mockResolvedValue({
        data: { success: true },
      });

      const result = await UserService.updateLastLogin("user-1");

      expect(result.success).toBe(true);
      expect(mockApiClient.put).toHaveBeenCalledWith(
        "/api/users/user-1",
        expect.objectContaining({
          lastLogin: expect.any(String),
        }),
      );
    });

    it("should handle update last login error", async () => {
      mockApiClient.put.mockResolvedValue({
        error: {
          message: "Failed to update",
          statusCode: 500,
        },
      });

      const result = await UserService.updateLastLogin("user-1");

      expect(result.success).toBe(false);
    });
  });

  describe("deleteUser", () => {
    it("should delete user successfully", async () => {
      mockApiClient.delete.mockResolvedValue({
        data: { success: true },
      });

      const result = await UserService.deleteUser("user-1");

      expect(result.success).toBe(true);
      expect(mockApiClient.delete).toHaveBeenCalledWith("/api/users/user-1");
    });

    it("should handle delete error", async () => {
      mockApiClient.delete.mockResolvedValue({
        error: {
          message: "User not found",
          statusCode: 404,
        },
      });

      const result = await UserService.deleteUser("non-existent");

      expect(result.success).toBe(false);
    });
  });
});
