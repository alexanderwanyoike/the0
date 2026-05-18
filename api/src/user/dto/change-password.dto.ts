import { IsNotEmpty, IsString, MinLength } from "class-validator";
import { MIN_PASSWORD_LENGTH } from "@/common/password-policy";

export class ChangePasswordDto {
  @IsString()
  @IsNotEmpty()
  currentPassword: string;

  @IsString()
  @MinLength(MIN_PASSWORD_LENGTH)
  newPassword: string;
}
