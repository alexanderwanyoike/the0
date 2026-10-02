import { Test, TestingModule } from "@nestjs/testing";
import {
  ServiceUnavailableException,
  UnauthorizedException,
} from "@nestjs/common";
import { AuthController } from "../auth.controller";
import { AuthService } from "../auth.service";
import { ApiKeyService } from "@/api-key/api-key.service";
import { Ok, Failure } from "../../common/result";

describe("AuthController", () => {
  let controller: AuthController;
  let authService: AuthService;

  const mockAuthService = {
    login: jest.fn(),
    validateToken: jest.fn(),
  };

  const mockApiKeyService = {
    // Add any methods the AuthController might use from ApiKeyService
    createApiKey: jest.fn(),
    getUserApiKeys: jest.fn(),
    deleteApiKey: jest.fn(),
    validateApiKey: jest.fn(),
  };

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      controllers: [AuthController],
      providers: [
        {
          provide: AuthService,
          useValue: mockAuthService,
        },
        {
          provide: ApiKeyService,
          useValue: mockApiKeyService,
        },
      ],
    }).compile();

    controller = module.get<AuthController>(AuthController);
    authService = module.get<AuthService>(AuthService);
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it("should be defined", () => {
    expect(controller).toBeDefined();
  });

  describe("login", () => {
    it("should return success on valid credentials", async () => {
      const loginDto = {
        email: "test@example.com",
        password: "password123",
      };

      const mockResult = Ok({
        token: "test-token",
        user: {
          id: "test-id",
          username: "testuser",
          email: "test@example.com",
          isActive: true,
          isEmailVerified: false,
          role: "user",
        },
      });

      mockAuthService.login.mockResolvedValue(mockResult);

      const result = await controller.login(loginDto);

      expect(result.success).toBe(true);
      expect(result.data.token).toBe("test-token");
      expect(authService.login).toHaveBeenCalledWith(loginDto);
    });

    it("should return error on invalid credentials", async () => {
      const loginDto = {
        email: "test@example.com",
        password: "wrongpassword",
      };

      const mockResult = Failure("Invalid credentials");
      mockAuthService.login.mockResolvedValue(mockResult);

      await expect(controller.login(loginDto)).rejects.toThrow(
        UnauthorizedException,
      );
      await expect(controller.login(loginDto)).rejects.toThrow(
        "Invalid credentials",
      );
    });
  });

  describe("validate", () => {
    it("should validate token successfully", async () => {
      const validateDto = { token: "valid-token" };

      const mockResult = Ok({
        id: "test-id",
        username: "testuser",
        email: "test@example.com",
        isActive: true,
        isEmailVerified: false,
        role: "user",
      });

      mockAuthService.validateToken.mockResolvedValue(mockResult);

      const result = await controller.validate(validateDto);

      expect(result.success).toBe(true);
      expect(authService.validateToken).toHaveBeenCalledWith("valid-token");
    });

    it("should return error for invalid token", async () => {
      const validateDto = { token: "invalid-token" };

      const mockResult = Failure("Invalid token");
      mockAuthService.validateToken.mockResolvedValue(mockResult);

      await expect(controller.validate(validateDto)).rejects.toThrow(
        UnauthorizedException,
      );
      await expect(controller.validate(validateDto)).rejects.toThrow(
        "Invalid token",
      );
    });
  });

  describe("validateApiKey", () => {
    const apiKey = {
      id: "key-1",
      userId: "user-1",
      name: "ci",
      key: "the0_secret",
      isActive: true,
      createdAt: new Date("2026-05-16T00:00:00Z"),
      updatedAt: new Date("2026-05-16T00:00:00Z"),
      lastUsedAt: new Date("2026-09-30T12:00:00Z"),
    };

    it("rejects a request without an Authorization header", async () => {
      await expect(controller.validateApiKey(undefined)).rejects.toThrow(
        new UnauthorizedException("Authorization header is required"),
      );
      expect(mockApiKeyService.validateApiKey).not.toHaveBeenCalled();
    });

    it("rejects an unsupported authorization scheme", async () => {
      await expect(
        controller.validateApiKey("Basic dXNlcjpwYXNz"),
      ).rejects.toThrow(
        new UnauthorizedException("Invalid authorization header format"),
      );
      expect(mockApiKeyService.validateApiKey).not.toHaveBeenCalled();
    });

    it.each(["ApiKey", "Bearer"])(
      "accepts a valid key sent with the %s scheme",
      async (scheme) => {
        mockApiKeyService.validateApiKey.mockResolvedValue(Ok(apiKey));

        const result = await controller.validateApiKey(`${scheme} the0_secret`);

        expect(mockApiKeyService.validateApiKey).toHaveBeenCalledWith(
          "the0_secret",
        );
        expect(result).toEqual({
          success: true,
          data: {
            valid: true,
            userId: "user-1",
            keyId: "key-1",
            keyName: "ci",
            lastUsedAt: "2026-09-30T12:00:00.000Z",
          },
          message: "API key is valid",
        });
      },
    );

    it("reports a key that was never used with a null lastUsedAt", async () => {
      mockApiKeyService.validateApiKey.mockResolvedValue(
        Ok({ ...apiKey, lastUsedAt: null }),
      );

      const result = await controller.validateApiKey("ApiKey the0_secret");

      expect(result.data.lastUsedAt).toBeNull();
    });

    it("rejects a key the service does not accept", async () => {
      mockApiKeyService.validateApiKey.mockResolvedValue(
        Failure("API key not found or inactive"),
      );

      await expect(
        controller.validateApiKey("ApiKey the0_unknown"),
      ).rejects.toThrow(UnauthorizedException);
    });

    it("answers a rejected key with a generic message that hides the reason", async () => {
      mockApiKeyService.validateApiKey.mockResolvedValue(
        Failure('relation "api_keys" does not exist at 10.0.0.5:5432'),
      );

      const error = await controller
        .validateApiKey("ApiKey the0_unknown")
        .catch((e: unknown) => e);

      expect(error).toEqual(new UnauthorizedException("Invalid API key"));
    });

    it("passes a database outage through as service unavailable", async () => {
      mockApiKeyService.validateApiKey.mockRejectedValue(
        new ServiceUnavailableException("Database temporarily unavailable"),
      );

      await expect(
        controller.validateApiKey("ApiKey the0_secret"),
      ).rejects.toThrow(ServiceUnavailableException);
    });
  });
});
