import { Test, TestingModule } from "@nestjs/testing";
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
});
