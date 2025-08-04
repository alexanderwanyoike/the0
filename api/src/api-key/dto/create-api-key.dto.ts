import { IsString, IsNotEmpty, MaxLength, MinLength } from 'class-validator';

export class CreateApiKeyDto {
  @IsString({ message: 'name must be a string' })
  @IsNotEmpty({ message: 'name should not be empty' })
  @MinLength(3, {
    message: 'name must be longer than or equal to 3 characters',
  })
  @MaxLength(50, {
    message: 'name must be shorter than or equal to 50 characters',
  })
  name: string;
}
