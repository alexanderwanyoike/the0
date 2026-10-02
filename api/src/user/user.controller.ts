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
  ApiBadRequestResponse,
  ApiNotFoundResponse,
  ApiOperation,
  ApiParam,
  ApiTags,
} from "@nestjs/swagger";
import { AdminJwtGuard } from "@/auth/admin-jwt.guard";
import { JwtAuthGuard } from "@/auth/jwt-auth.guard";
import { CurrentUser } from "@/auth/current-user.decorator";
import { AuthenticatedUser } from "@/auth/auth.types";
import { UserService } from "./user.service";
import { ChangePasswordDto } from "./dto/change-password.dto";
import { CreateAdminUserDto } from "./dto/create-admin-user.dto";
import { DeleteAccountDto } from "./dto/delete-account.dto";
import { ResetPasswordDto } from "./dto/reset-password.dto";
import { UpdateAdminUserDto } from "./dto/update-admin-user.dto";
import { UpdateProfileDto } from "./dto/update-profile.dto";
import { ApiAdminJwtAuth, ApiJwtAuth } from "@/swagger/api-auth.decorators";

const USER_ID_PARAM = { name: "id", description: "User ID" };

@Controller()
export class UserController {
  constructor(private readonly userService: UserService) {}

  @Get("admin/users")
  @UseGuards(AdminJwtGuard)
  @ApiTags("admin")
  @ApiAdminJwtAuth()
  @ApiOperation({ summary: "List all users" })
  async listUsers() {
    return {
      success: true,
      data: await this.userService.listUsers(),
      message: "Users retrieved successfully",
    };
  }

  @Post("admin/users")
  @UseGuards(AdminJwtGuard)
  @ApiTags("admin")
  @ApiAdminJwtAuth()
  @ApiOperation({ summary: "Create a user" })
  @ApiBadRequestResponse({ description: "Invalid user details" })
  async createUser(@Body() body: CreateAdminUserDto) {
    return {
      success: true,
      data: await this.userService.createUser(body),
      message: "User created successfully",
    };
  }

  @Patch("admin/users/:id")
  @UseGuards(AdminJwtGuard)
  @ApiTags("admin")
  @ApiAdminJwtAuth()
  @ApiOperation({ summary: "Update a user's details, role or active state" })
  @ApiParam(USER_ID_PARAM)
  @ApiNotFoundResponse({ description: "User not found" })
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
  @ApiTags("admin")
  @ApiAdminJwtAuth()
  @ApiOperation({ summary: "Set a new password for a user" })
  @ApiParam(USER_ID_PARAM)
  @ApiNotFoundResponse({ description: "User not found" })
  async resetPassword(
    @Param("id") id: string,
    @Body() body: ResetPasswordDto,
    @CurrentUser() user: AuthenticatedUser,
  ) {
    return {
      success: true,
      data: await this.userService.resetPassword(id, body.password, user),
      message: "Password reset successfully",
    };
  }

  @Put("users/profile")
  @UseGuards(JwtAuthGuard)
  @ApiTags("users")
  @ApiJwtAuth()
  @ApiOperation({ summary: "Update your profile" })
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
  @ApiTags("users")
  @ApiJwtAuth()
  @ApiOperation({ summary: "Change your password" })
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
  @ApiTags("users")
  @ApiJwtAuth()
  @ApiOperation({
    summary: "Deactivate your account",
    description: "Requires your current password in the body.",
  })
  async deleteAccount(
    @CurrentUser() user: AuthenticatedUser,
    @Body() body: DeleteAccountDto,
  ) {
    return {
      success: true,
      data: await this.userService.deleteAccount(user, body.password),
      message: "Account deactivated successfully",
    };
  }
}
