import {
  ExecutionContext,
  ForbiddenException,
  UnauthorizedException,
} from "@nestjs/common";
import { Test, TestingModule } from "@nestjs/testing";
import { USER_ROLES } from "@/user/user.constants";
import { Ok, Failure } from "@/common/result";
import { AdminJwtGuard } from "../admin-jwt.guard";
import { AuthService, AuthUser } from "../auth.service";

describe("AdminJwtGuard", () => {
  let guard: AdminJwtGuard;

  const mockAuthService = {
    validateToken: jest.fn(),
  };

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [
        AdminJwtGuard,
        {
          provide: AuthService,
          useValue: mockAuthService,
        },
      ],
    }).compile();

    guard = module.get<AdminJwtGuard>(AdminJwtGuard);
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  function contextWithAuthHeader(authorization?: string) {
    const request = {
      headers: authorization ? { authorization } : {},
    };
    return {
      request,
      context: {
        switchToHttp: () => ({
          getRequest: () => request,
        }),
      } as ExecutionContext,
    };
  }

  const adminUser: AuthUser = {
    id: "admin-1",
    username: "admin",
    email: "admin@example.com",
    isActive: true,
    isEmailVerified: true,
    role: USER_ROLES.ADMIN,
  };

  it("allows admin JWTs", async () => {
    const { context, request } = contextWithAuthHeader("Bearer admin-token");
    mockAuthService.validateToken.mockResolvedValue(Ok(adminUser));

    await expect(guard.canActivate(context)).resolves.toBe(true);

    expect(mockAuthService.validateToken).toHaveBeenCalledWith("admin-token");
    expect(request).toMatchObject({
      user: {
        id: adminUser.id,
        role: USER_ROLES.ADMIN,
        authType: "jwt",
      },
    });
  });

  it("rejects missing tokens", async () => {
    const { context } = contextWithAuthHeader();

    await expect(guard.canActivate(context)).rejects.toThrow(
      UnauthorizedException,
    );
  });

  it("rejects invalid tokens", async () => {
    const { context } = contextWithAuthHeader("Bearer invalid-token");
    mockAuthService.validateToken.mockResolvedValue(Failure("Invalid token"));

    await expect(guard.canActivate(context)).rejects.toThrow(
      UnauthorizedException,
    );
  });

  it("rejects non-admin JWTs", async () => {
    const { context } = contextWithAuthHeader("Bearer user-token");
    mockAuthService.validateToken.mockResolvedValue(
      Ok({ ...adminUser, role: USER_ROLES.USER }),
    );

    await expect(guard.canActivate(context)).rejects.toThrow(
      ForbiddenException,
    );
  });
});
