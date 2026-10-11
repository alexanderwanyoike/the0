import { Test, TestingModule } from "@nestjs/testing";
import {
  ExecutionContext,
  ServiceUnavailableException,
  UnauthorizedException,
} from "@nestjs/common";
import { AuthCombinedGuard } from "../auth-combined.guard";
import { AuthService } from "../auth.service";
import { ApiKeyService } from "../../api-key/api-key.service";
import { Ok, Failure } from "../../common/result";

describe("AuthCombinedGuard", () => {
  let guard: AuthCombinedGuard;
  let authService: AuthService;
  let apiKeyService: ApiKeyService;

  const mockAuthService = {
    validateToken: jest.fn(),
  };

  const mockApiKeyService = {
    validateApiKey: jest.fn(),
  };

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [
        AuthCombinedGuard,
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

    guard = module.get<AuthCombinedGuard>(AuthCombinedGuard);
    authService = module.get<AuthService>(AuthService);
    apiKeyService = module.get<ApiKeyService>(ApiKeyService);
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  const contextFor = (request: Record<string, unknown>) =>
    ({
      switchToHttp: () => ({ getRequest: () => request }),
    }) as ExecutionContext;

  it("should be defined", () => {
    expect(guard).toBeDefined();
  });

  it("should allow access with valid JWT token", async () => {
    const mockContext = {
      switchToHttp: () => ({
        getRequest: () => ({
          headers: {
            authorization: "Bearer valid-jwt-token",
          },
        }),
      }),
    } as ExecutionContext;

    const mockUser = {
      id: "test-id",
      username: "testuser",
      email: "test@example.com",
      isActive: true,
      isEmailVerified: false,
      role: "user",
    };

    mockAuthService.validateToken.mockResolvedValue(Ok(mockUser));

    const result = await guard.canActivate(mockContext);

    expect(result).toBe(true);
    expect(authService.validateToken).toHaveBeenCalledWith("valid-jwt-token");
  });

  it("should throw exception without JWT token", async () => {
    const mockContext = {
      switchToHttp: () => ({
        getRequest: () => ({
          headers: {
            "x-api-key": "valid-api-key",
          },
        }),
      }),
    } as ExecutionContext;

    await expect(guard.canActivate(mockContext)).rejects.toThrow(
      "Authentication required. Provide Bearer JWT token or ApiKey.",
    );
  });

  it("should throw exception without any authentication", async () => {
    const mockContext = {
      switchToHttp: () => ({
        getRequest: () => ({
          headers: {},
        }),
      }),
    } as ExecutionContext;

    await expect(guard.canActivate(mockContext)).rejects.toThrow(
      "Authentication required. Provide Bearer JWT token or ApiKey.",
    );
  });

  describe("the Actor it attaches", () => {
    it("a JWT gives a jwt Actor for the token's user", async () => {
      const request = { headers: { authorization: "Bearer valid-jwt-token" } };
      mockAuthService.validateToken.mockResolvedValue(
        Ok({ id: "user-1", username: "u", email: "u@example.com" }),
      );

      await guard.canActivate(contextFor(request));

      expect(request).toMatchObject({
        actor: { userId: "user-1", via: "jwt" },
      });
    });

    it("an ApiKey gives an api-key Actor naming the key", async () => {
      const request = { headers: { authorization: "ApiKey the0_secret" } };
      mockApiKeyService.validateApiKey.mockResolvedValue(
        Ok({ id: "key-1", userId: "user-1", name: "ci" }),
      );

      await guard.canActivate(contextFor(request));

      expect(request).toMatchObject({
        actor: { userId: "user-1", via: "api-key", apiKeyId: "key-1" },
      });
    });
  });

  describe("ApiKey authentication", () => {
    it("accepts a valid key and attaches its owner to the request", async () => {
      const request = { headers: { authorization: "ApiKey the0_secret" } };
      mockApiKeyService.validateApiKey.mockResolvedValue(
        Ok({ id: "key-1", userId: "user-1", name: "ci" }),
      );

      await expect(guard.canActivate(contextFor(request))).resolves.toBe(true);
      expect(mockApiKeyService.validateApiKey).toHaveBeenCalledWith(
        "the0_secret",
      );
      expect(request).toMatchObject({ user: { uid: "user-1" } });
    });

    it("answers a rejected key with a generic message that hides the reason", async () => {
      mockApiKeyService.validateApiKey.mockResolvedValue(
        Failure('relation "api_keys" does not exist at 10.0.0.5:5432'),
      );

      const error = await guard
        .canActivate(
          contextFor({ headers: { authorization: "ApiKey the0_unknown" } }),
        )
        .catch((e: unknown) => e);

      expect(error).toEqual(new UnauthorizedException("Invalid API key"));
    });

    it("passes a database outage through as service unavailable", async () => {
      mockApiKeyService.validateApiKey.mockRejectedValue(
        new ServiceUnavailableException("Database temporarily unavailable"),
      );

      await expect(
        guard.canActivate(
          contextFor({ headers: { authorization: "ApiKey the0_secret" } }),
        ),
      ).rejects.toThrow(ServiceUnavailableException);
    });
  });
});
