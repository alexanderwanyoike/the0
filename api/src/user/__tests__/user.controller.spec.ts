import { BadRequestException, ForbiddenException } from "@nestjs/common";
import { GUARDS_METADATA } from "@nestjs/common/constants";
import { plainToInstance } from "class-transformer";
import { validate } from "class-validator";
import { AdminJwtGuard } from "@/auth/admin-jwt.guard";
import { AuthenticatedUser } from "@/auth/auth.types";
import { JwtAuthGuard } from "@/auth/jwt-auth.guard";
import { USER_ROLES } from "../user.constants";
import { CreateAdminUserDto } from "../dto/create-admin-user.dto";
import { UserController } from "../user.controller";
import { UserService } from "../user.service";
import { SerializedUser } from "../user.types";

describe("UserController", () => {
  let controller: UserController;
  let userService: jest.Mocked<UserService>;

  const actor: AuthenticatedUser = {
    uid: "admin-1",
    id: "admin-1",
    username: "admin",
    email: "admin@example.com",
    firstName: null,
    lastName: null,
    role: USER_ROLES.ADMIN,
    isActive: true,
    isEmailVerified: true,
    authType: "jwt" as const,
  };

  const managedUser: SerializedUser = {
    id: "user-1",
    username: "user",
    email: "user@example.com",
    firstName: null,
    lastName: null,
    role: USER_ROLES.USER,
    isActive: true,
    isEmailVerified: true,
    lastLoginAt: null,
    createdAt: new Date("2026-05-17T00:00:00Z"),
    updatedAt: new Date("2026-05-17T00:00:00Z"),
  };

  beforeEach(() => {
    userService = {
      listUsers: jest.fn(),
      createUser: jest.fn(),
      updateUser: jest.fn(),
      resetPassword: jest.fn(),
      updateProfile: jest.fn(),
      changePassword: jest.fn(),
      deleteAccount: jest.fn(),
    } as unknown as jest.Mocked<UserService>;

    controller = new UserController(userService);
  });

  function methodGuards(methodName: keyof UserController) {
    return Reflect.getMetadata(
      GUARDS_METADATA,
      UserController.prototype[methodName],
    );
  }

  it("guards admin user management routes with AdminJwtGuard", () => {
    expect(methodGuards("listUsers")).toContain(AdminJwtGuard);
    expect(methodGuards("createUser")).toContain(AdminJwtGuard);
    expect(methodGuards("updateUser")).toContain(AdminJwtGuard);
    expect(methodGuards("resetPassword")).toContain(AdminJwtGuard);
  });

  it("guards self-service routes with JwtAuthGuard", () => {
    expect(methodGuards("updateProfile")).toContain(JwtAuthGuard);
    expect(methodGuards("changePassword")).toContain(JwtAuthGuard);
    expect(methodGuards("deleteAccount")).toContain(JwtAuthGuard);
  });

  it("returns wrapped admin user list responses", async () => {
    userService.listUsers.mockResolvedValue([managedUser]);

    await expect(controller.listUsers()).resolves.toEqual({
      success: true,
      data: [managedUser],
      message: "Users retrieved successfully",
    });
  });

  it("returns wrapped create user responses", async () => {
    userService.createUser.mockResolvedValue(managedUser);

    await expect(
      controller.createUser({
        email: managedUser.email,
        password: "password123",
      }),
    ).resolves.toEqual({
      success: true,
      data: managedUser,
      message: "User created successfully",
    });
  });

  it("propagates create user errors", async () => {
    userService.createUser.mockRejectedValue(
      new BadRequestException("Email is already in use"),
    );

    await expect(
      controller.createUser({
        email: managedUser.email,
        password: "password123",
      }),
    ).rejects.toThrow("Email is already in use");
  });

  it("returns wrapped update user responses", async () => {
    userService.updateUser.mockResolvedValue({
      ...managedUser,
      role: USER_ROLES.ADMIN,
    });

    await expect(
      controller.updateUser(managedUser.id, { role: USER_ROLES.ADMIN }, actor),
    ).resolves.toMatchObject({
      success: true,
      data: { id: managedUser.id, role: USER_ROLES.ADMIN },
      message: "User updated successfully",
    });
  });

  it("propagates forbidden reset-password self actions", async () => {
    userService.resetPassword.mockRejectedValue(
      new ForbiddenException("Use change password for your own admin account"),
    );

    await expect(
      controller.resetPassword(actor.id, { password: "password123" }, actor),
    ).rejects.toThrow("Use change password for your own admin account");
  });

  it("validates create-user DTOs", async () => {
    const dto = plainToInstance(CreateAdminUserDto, {
      email: "not-an-email",
      password: "short",
      username: "",
    });

    const errors = await validate(dto);

    expect(errors.map((error) => error.property)).toEqual(
      expect.arrayContaining(["email", "password", "username"]),
    );
  });
});
