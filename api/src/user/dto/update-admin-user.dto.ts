import { Transform } from "class-transformer";
import {
  IsBoolean,
  IsEmail,
  IsIn,
  IsNotEmpty,
  IsOptional,
  IsString,
} from "class-validator";
import { USER_ROLE_VALUES, UserRole } from "../user.constants";

export class UpdateAdminUserDto {
  @Transform(({ value }) => (typeof value === "string" ? value.trim() : value))
  @IsString()
  @IsNotEmpty()
  @IsOptional()
  username?: string;

  @Transform(({ value }) => (typeof value === "string" ? value.trim() : value))
  @IsEmail()
  @IsOptional()
  email?: string;

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
