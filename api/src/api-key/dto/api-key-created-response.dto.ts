import { ApiKeyResponseDto } from '@/api-key/dto/api-key-response.dto';

export class ApiKeyCreatedResponseDto extends ApiKeyResponseDto {
  key: string; // Full key only returned on creation
}
