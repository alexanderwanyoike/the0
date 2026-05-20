import { IsString, MinLength } from "class-validator";
import { MIN_PASSWORD_LENGTH } from "@/common/password-policy";

export class ResetPasswordDto {
  @IsString()
  @MinLength(MIN_PASSWORD_LENGTH)
  password: string;
}
