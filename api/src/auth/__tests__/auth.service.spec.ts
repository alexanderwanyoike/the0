import { Test, TestingModule } from "@nestjs/testing";
import { ServiceUnavailableException } from "@nestjs/common";
import { AuthService } from "../auth.service";
import { JwtService } from "@nestjs/jwt";

jest.mock("../../database/connection", () => ({
  getDatabase: jest.fn().mockReturnValue({
    select: jest.fn().mockReturnValue({
      from: jest.fn().mockReturnValue({
        where: jest.fn().mockReturnValue([
          {
            id: "test-id",
            username: "testuser",
            email: "test@example.com",
            firstName: "Test",
            lastName: "User",
            isActive: true,
            isEmailVerified: false,
          },
        ]),
      }),
    }),
    insert: jest.fn().mockReturnValue({
      values: jest.fn().mockReturnValue({
        returning: jest.fn().mockReturnValue([
          {
            id: "test-id",
            username: "testuser",
            email: "test@example.com",
            isActive: true,
            isEmailVerified: false,
          },
        ]),
      }),
    }),
  }),
  getDatabaseConfig: jest.fn().mockReturnValue({ type: "sqlite" }),
}));

jest.mock("bcrypt", () => ({
  hash: jest.fn().mockResolvedValue("hashed-password"),
  compare: jest.fn().mockResolvedValue(true),
}));

describe("AuthService", () => {
  let service: AuthService;
  let jwtService: JwtService;

  beforeEach(async () => {
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
      ],
    }).compile();

    service = module.get<AuthService>(AuthService);
    jwtService = module.get<JwtService>(JwtService);
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
      const { getDatabase } = require("../../database/connection");
      (getDatabase as jest.Mock).mockReturnValue({
        select: jest.fn().mockReturnValue({
          from: jest.fn().mockReturnValue({
            where: jest.fn().mockRejectedValue(error),
          }),
        }),
      });
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
