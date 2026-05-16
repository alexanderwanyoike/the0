import {
  Body,
  Controller,
  Delete,
  Get,
  Param,
  Patch,
  Post,
  Put,
  UseGuards,
} from "@nestjs/common";
import {
  IsBoolean,
  IsEmail,
  IsIn,
  IsNotEmpty,
  IsOptional,
  IsString,
  MinLength,
} from "class-validator";
import { AdminJwtGuard } from "@/auth/admin-jwt.guard";
import { JwtAuthGuard } from "@/auth/jwt-auth.guard";
import { CurrentUser } from "@/auth/current-user.decorator";
import { AuthenticatedUser } from "@/auth/auth.types";
import { UserService } from "./user.service";

class CreateAdminUserDto {
  @IsString()
  @IsOptional()
  username?: string;

  @IsEmail()
  email: string;

  @IsString()
  @MinLength(6)
  password: string;

  @IsString()
  @IsOptional()
  firstName?: string;

  @IsString()
  @IsOptional()
  lastName?: string;

  @IsIn(["admin", "user"])
  @IsOptional()
  role?: "admin" | "user";

  @IsBoolean()
  @IsOptional()
  isActive?: boolean;
}

class UpdateAdminUserDto {
  @IsString()
  @IsOptional()
  username?: string;

  @IsEmail()
  @IsOptional()
  email?: string;

  @IsString()
  @IsOptional()
  firstName?: string;

  @IsString()
  @IsOptional()
  lastName?: string;

  @IsIn(["admin", "user"])
  @IsOptional()
  role?: "admin" | "user";

  @IsBoolean()
  @IsOptional()
  isActive?: boolean;
}

class ResetPasswordDto {
  @IsString()
  @MinLength(6)
  password: string;
}

class UpdateProfileDto {
  @IsString()
  @IsOptional()
  username?: string;

  @IsString()
  @IsOptional()
  firstName?: string;

  @IsString()
  @IsOptional()
  lastName?: string;
}

class ChangePasswordDto {
  @IsString()
  @IsNotEmpty()
  currentPassword: string;

  @IsString()
  @MinLength(6)
  newPassword: string;
}

class DeleteAccountDto {
  @IsString()
  @IsNotEmpty()
  password: string;
}

@Controller()
export class UserController {
  constructor(private readonly userService: UserService) {}

  @Get("admin/users")
  @UseGuards(AdminJwtGuard)
  async listUsers() {
    return {
      success: true,
      data: await this.userService.listUsers(),
      message: "Users retrieved successfully",
    };
  }

  @Post("admin/users")
  @UseGuards(AdminJwtGuard)
  async createUser(@Body() body: CreateAdminUserDto) {
    return {
      success: true,
      data: await this.userService.createUser(body),
      message: "User created successfully",
    };
  }

  @Patch("admin/users/:id")
  @UseGuards(AdminJwtGuard)
  async updateUser(
    @Param("id") id: string,
    @Body() body: UpdateAdminUserDto,
    @CurrentUser() user: AuthenticatedUser,
  ) {
    return {
      success: true,
      data: await this.userService.updateUser(id, body, user),
      message: "User updated successfully",
    };
  }

  @Post("admin/users/:id/reset-password")
  @UseGuards(AdminJwtGuard)
  async resetPassword(@Param("id") id: string, @Body() body: ResetPasswordDto) {
    return {
      success: true,
      data: await this.userService.resetPassword(id, body.password),
      message: "Password reset successfully",
    };
  }

  @Put("users/profile")
  @UseGuards(JwtAuthGuard)
  async updateProfile(
    @CurrentUser() user: AuthenticatedUser,
    @Body() body: UpdateProfileDto,
  ) {
    return {
      success: true,
      data: await this.userService.updateProfile(user, body),
      message: "Profile updated successfully",
    };
  }

  @Put("users/change-password")
  @UseGuards(JwtAuthGuard)
  async changePassword(
    @CurrentUser() user: AuthenticatedUser,
    @Body() body: ChangePasswordDto,
  ) {
    return {
      success: true,
      data: await this.userService.changePassword(
        user,
        body.currentPassword,
        body.newPassword,
      ),
      message: "Password changed successfully",
    };
  }

  @Delete("users/delete-account")
  @UseGuards(JwtAuthGuard)
  async deleteAccount(
    @CurrentUser() user: AuthenticatedUser,
    @Body() body: DeleteAccountDto,
  ) {
    return {
      success: true,
      data: await this.userService.deleteAccount(user, body.password),
      message: "Account deleted successfully",
    };
  }
}
