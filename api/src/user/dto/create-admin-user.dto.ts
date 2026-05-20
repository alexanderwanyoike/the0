import { Transform } from "class-transformer";
import {
  IsBoolean,
  IsEmail,
  IsIn,
  IsNotEmpty,
  IsOptional,
  IsString,
  MinLength,
} from "class-validator";
import { MIN_PASSWORD_LENGTH } from "@/common/password-policy";
import { USER_ROLE_VALUES, UserRole } from "../user.constants";

export class CreateAdminUserDto {
  @Transform(({ value }) => (typeof value === "string" ? value.trim() : value))
  @IsString()
  @IsNotEmpty()
  @IsOptional()
  username?: string;

  @Transform(({ value }) => (typeof value === "string" ? value.trim() : value))
  @IsEmail()
  email: string;

  @IsString()
  @MinLength(MIN_PASSWORD_LENGTH)
  password: string;

  @Transform(({ value }) => (typeof value === "string" ? value.trim() : value))
  @IsString()
  @IsOptional()
  firstName?: string;

  @Transform(({ value }) => (typeof value === "string" ? value.trim() : value))
  @IsString()
  @IsOptional()
  lastName?: string;

  @IsIn(USER_ROLE_VALUES)
  @IsOptional()
  role?: UserRole;

  @IsBoolean()
  @IsOptional()
  isActive?: boolean;
}
