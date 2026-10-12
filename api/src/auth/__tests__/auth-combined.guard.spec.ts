import { Test, TestingModule } from "@nestjs/testing";
import {
  ExecutionContext,
  ServiceUnavailableException,
  UnauthorizedException,
} from "@nestjs/common";
import { AuthCombinedGuard } from "../auth-combined.guard";
import { ActorVia, AllowAnonymous } from "../actor.decorators";
import { AuthService } from "../auth.service";
import { ApiKeyService } from "../../api-key/api-key.service";
import { Ok, Failure } from "../../common/result";

class RestRoute {
  handle() {}
}

@ActorVia("mcp")
class McpRoute {
  handle() {}
}

@AllowAnonymous()
class AnonymousRoute {
  handle() {}
}

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

  const contextFor = (
    request: Record<string, unknown>,
    route: { new (): { handle(): void } } = RestRoute,
  ) =>
    ({
      switchToHttp: () => ({ getRequest: () => request }),
      getHandler: () => route.prototype.handle,
      getClass: () => route,
    }) as unknown as ExecutionContext;

  it("should be defined", () => {
    expect(guard).toBeDefined();
  });

  it("should allow access with valid JWT token", async () => {
    const mockContext = contextFor({
      headers: {
        authorization: "Bearer valid-jwt-token",
      },
    });

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

  it("should throw exception without any authentication", async () => {
    const mockContext = contextFor({ headers: {} });

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

    it("an x-api-key gives an api-key Actor naming the key", async () => {
      const request = { headers: { "x-api-key": "the0_secret" } };
      mockApiKeyService.validateApiKey.mockResolvedValue(
        Ok({ id: "key-1", userId: "user-1", name: "ci" }),
      );

      await guard.canActivate(contextFor(request));

      expect(request).toMatchObject({
        actor: { userId: "user-1", via: "api-key", apiKeyId: "key-1" },
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

  describe("on a route whose Actors arrive through MCP", () => {
    beforeEach(() => {
      mockApiKeyService.validateApiKey.mockResolvedValue(
        Ok({ id: "key-1", userId: "user-1", name: "agent" }),
      );
      mockAuthService.validateToken.mockResolvedValue(
        Ok({ id: "user-1", username: "u", email: "u@example.com" }),
      );
    });

    it.each([
      { header: "x-api-key", headers: { "x-api-key": "the0_secret" } },
      {
        header: "Authorization: ApiKey",
        headers: { authorization: "ApiKey the0_secret" },
      },
    ])(
      "gives a key sent in $header an mcp Actor naming the key",
      async ({ headers }) => {
        const request = { headers };

        await guard.canActivate(contextFor(request, McpRoute));

        expect(request).toMatchObject({
          actor: { userId: "user-1", via: "mcp", apiKeyId: "key-1" },
        });
      },
    );

    it("gives a JWT an mcp Actor", async () => {
      const request = { headers: { authorization: "Bearer valid-jwt-token" } };

      await guard.canActivate(contextFor(request, McpRoute));

      expect(request).toMatchObject({
        actor: { userId: "user-1", via: "mcp" },
      });
    });

    it("still rejects a request without credentials", async () => {
      await expect(
        guard.canActivate(contextFor({ headers: {} }, McpRoute)),
      ).rejects.toThrow(UnauthorizedException);
    });
  });

  describe("on a route that allows anonymous callers", () => {
    it("lets a request without credentials through with no Actor", async () => {
      const request = { headers: {} };

      await expect(
        guard.canActivate(contextFor(request, AnonymousRoute)),
      ).resolves.toBe(true);
      expect(request).not.toHaveProperty("actor");
    });

    it("lets a request whose credentials do not validate through with no Actor", async () => {
      mockApiKeyService.validateApiKey.mockResolvedValue(
        Failure("API key not found or inactive"),
      );
      mockAuthService.validateToken.mockResolvedValue(Failure("Invalid token"));
      const badKey = { headers: { "x-api-key": "the0_unknown" } };
      const badJwt = { headers: { authorization: "Bearer expired" } };

      await expect(
        guard.canActivate(contextFor(badKey, AnonymousRoute)),
      ).resolves.toBe(true);
      await expect(
        guard.canActivate(contextFor(badJwt, AnonymousRoute)),
      ).resolves.toBe(true);
      expect(badKey).not.toHaveProperty("actor");
      expect(badJwt).not.toHaveProperty("actor");
    });

    it("still attaches the Actor for valid credentials", async () => {
      mockApiKeyService.validateApiKey.mockResolvedValue(
        Ok({ id: "key-1", userId: "user-1", name: "agent" }),
      );
      const request = { headers: { "x-api-key": "the0_secret" } };

      await guard.canActivate(contextFor(request, AnonymousRoute));

      expect(request).toMatchObject({
        actor: { userId: "user-1", via: "api-key", apiKeyId: "key-1" },
      });
    });

    it("passes a database outage through as service unavailable", async () => {
      mockApiKeyService.validateApiKey.mockRejectedValue(
        new ServiceUnavailableException("Database temporarily unavailable"),
      );

      await expect(
        guard.canActivate(
          contextFor(
            { headers: { "x-api-key": "the0_secret" } },
            AnonymousRoute,
          ),
        ),
      ).rejects.toThrow(ServiceUnavailableException);
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

    it("accepts a key sent in the x-api-key header", async () => {
      const request = { headers: { "x-api-key": "the0_secret" } };
      mockApiKeyService.validateApiKey.mockResolvedValue(
        Ok({ id: "key-1", userId: "user-1", name: "ci" }),
      );

      await expect(guard.canActivate(contextFor(request))).resolves.toBe(true);
      expect(mockApiKeyService.validateApiKey).toHaveBeenCalledWith(
        "the0_secret",
      );
      expect(request).toMatchObject({ user: { uid: "user-1" } });
    });

    it("uses x-api-key when an Authorization header is also sent", async () => {
      mockApiKeyService.validateApiKey.mockResolvedValue(
        Ok({ id: "key-1", userId: "user-1", name: "ci" }),
      );

      await guard.canActivate(
        contextFor({
          headers: {
            authorization: "ApiKey the0_authorization",
            "x-api-key": "the0_header",
          },
        }),
      );

      expect(mockApiKeyService.validateApiKey).toHaveBeenCalledTimes(1);
      expect(mockApiKeyService.validateApiKey).toHaveBeenCalledWith(
        "the0_header",
      );
    });

    it("authenticates a valid x-api-key sent beside a stale Bearer token", async () => {
      mockApiKeyService.validateApiKey.mockResolvedValue(
        Ok({ id: "key-1", userId: "user-1", name: "ci" }),
      );
      mockAuthService.validateToken.mockResolvedValue(Failure("Invalid token"));
      const request = {
        headers: { authorization: "Bearer stale", "x-api-key": "the0_secret" },
      };

      await expect(guard.canActivate(contextFor(request))).resolves.toBe(true);
      expect(mockAuthService.validateToken).not.toHaveBeenCalled();
      expect(request).toMatchObject({
        actor: { userId: "user-1", via: "api-key", apiKeyId: "key-1" },
      });
    });

    it("rejects an x-api-key that does not validate", async () => {
      mockApiKeyService.validateApiKey.mockResolvedValue(
        Failure("API key not found or inactive"),
      );

      const error = await guard
        .canActivate(contextFor({ headers: { "x-api-key": "the0_unknown" } }))
        .catch((e: unknown) => e);

      expect(error).toEqual(new UnauthorizedException("Invalid API key"));
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
