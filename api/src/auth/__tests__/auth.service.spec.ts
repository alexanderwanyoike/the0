import { Test, TestingModule } from "@nestjs/testing";
import { ServiceUnavailableException } from "@nestjs/common";
import { AuthService } from "../auth.service";
import { JwtService } from "@nestjs/jwt";
import { SetupLockRepository } from "../setup-lock.repository";
import { UserRepository } from "@/user/user.repository";
import { USER_ROLES } from "@/user/user.constants";
import { UserRecord } from "@/user/user.types";

jest.mock("bcrypt", () => ({
  hash: jest.fn().mockResolvedValue("hashed-password"),
  compare: jest.fn().mockResolvedValue(true),
}));

describe("AuthService", () => {
  let service: AuthService;
  let jwtService: JwtService;
  let userRepository: jest.Mocked<UserRepository>;

  const testUser: UserRecord = {
    id: "test-id",
    username: "testuser",
    email: "test@example.com",
    passwordHash: "hashed-password",
    firstName: "Test",
    lastName: "User",
    role: USER_ROLES.USER,
    sessionVersion: 0,
    isActive: true,
    isEmailVerified: false,
    lastLoginAt: null,
    metadata: {},
    createdAt: new Date("2026-05-16T00:00:00Z"),
    updatedAt: new Date("2026-05-16T00:00:00Z"),
  };

  beforeEach(async () => {
    const mockUserRepository = {
      findById: jest.fn().mockResolvedValue(testUser),
      findByEmail: jest.fn().mockResolvedValue(testUser),
      updateLastLogin: jest.fn().mockResolvedValue(undefined),
      count: jest.fn(),
      hasActiveAdmin: jest.fn(),
      createFirstAdmin: jest.fn(),
    };

    const module: TestingModule = await Test.createTestingModule({
      providers: [
        AuthService,
        {
          provide: JwtService,
          useValue: {
            verify: jest
              .fn()
              .mockReturnValue({ sub: "test-id", username: "testuser" }),
            sign: jest.fn().mockReturnValue("test-token"),
          },
        },
        {
          provide: UserRepository,
          useValue: mockUserRepository,
        },
        {
          provide: SetupLockRepository,
          useValue: {
            withLock: jest.fn(),
          },
        },
      ],
    }).compile();

    service = module.get<AuthService>(AuthService);
    jwtService = module.get<JwtService>(JwtService);
    userRepository = module.get(UserRepository);
  });

  it("should be defined", () => {
    expect(service).toBeDefined();
  });

  it("should validate JWT token successfully", async () => {
    const token = "valid-token";
    const result = await service.validateToken(token);

    expect(result.success).toBe(true);
    expect(jwtService.verify).toHaveBeenCalledWith(token);
  });

  it("should handle invalid JWT token", async () => {
    const token = "invalid-token";
    (jwtService.verify as jest.Mock).mockImplementation(() => {
      throw new Error("Invalid token");
    });

    const result = await service.validateToken(token);

    expect(result.success).toBe(false);
    expect(result.error).toContain("Invalid token");
  });

  describe("validateToken - DB connection errors", () => {
    function mockDbError(error: Error) {
      userRepository.findById.mockRejectedValue(error);
    }

    beforeEach(() => {
      (jwtService.verify as jest.Mock).mockReturnValue({
        sub: "test-id",
        username: "testuser",
      });
    });

    it("should throw ServiceUnavailableException on ETIMEDOUT", async () => {
      const error = Object.assign(new Error("Connection timed out"), {
        code: "ETIMEDOUT",
      });
      mockDbError(error);

      await expect(service.validateToken("valid-token")).rejects.toThrow(
        ServiceUnavailableException,
      );
    });

    it("should throw ServiceUnavailableException on ENETUNREACH", async () => {
      const error = Object.assign(new Error("Network unreachable"), {
        code: "ENETUNREACH",
      });
      mockDbError(error);

      await expect(service.validateToken("valid-token")).rejects.toThrow(
        ServiceUnavailableException,
      );
    });

    it("should throw ServiceUnavailableException on CONNECT_TIMEOUT", async () => {
      const error = Object.assign(new Error("Connect timeout"), {
        code: "CONNECT_TIMEOUT",
      });
      mockDbError(error);

      await expect(service.validateToken("valid-token")).rejects.toThrow(
        ServiceUnavailableException,
      );
    });

    it("should throw ServiceUnavailableException on AggregateError wrapping connection errors", async () => {
      const innerError = Object.assign(new Error("Connection timed out"), {
        code: "ETIMEDOUT",
      });
      const aggError = new AggregateError([innerError], "Multiple failures");
      mockDbError(aggError);

      await expect(service.validateToken("valid-token")).rejects.toThrow(
        ServiceUnavailableException,
      );
    });

    it('should still return "Invalid token" for non-connection errors', async () => {
      const error = new Error("some other DB error");
      mockDbError(error);

      const result = await service.validateToken("valid-token");

      expect(result.success).toBe(false);
      expect(result.error).toBe("Invalid token");
    });
  });
});
