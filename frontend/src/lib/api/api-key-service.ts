import axios from '@/lib/axios-interceptor';
import { Result, Ok, Failure } from '@/lib/result';
import { getErrorMessage, getErrorStatusCode } from '@/lib/axios';

export interface ApiKey {
  id: string;
  userId: string;
  name: string;
  key: string;
  isActive: boolean;
  createdAt: string;
  updatedAt: string;
  lastUsedAt?: string;
}

export interface CreateApiKeyRequest {
  name: string;
}

export interface ApiKeyStats {
  total: number;
  active: number;
}

export type ApiKeyServiceError = {
  message: string;
  statusCode: number;
};

export class ApiKeyService {
  public static async getApiKeys(): Promise<
    Result<ApiKey[], ApiKeyServiceError>
  > {
    try {
      const response = await axios.get('/api/api-keys', {
        timeout: 10000,
      });
      return Ok(response.data);
    } catch (error: any) {
      return this.handleError(error);
    }
  }

  public static async createApiKey(
    request: CreateApiKeyRequest,
  ): Promise<Result<ApiKey, ApiKeyServiceError>> {
    try {
      const response = await axios.post('/api/api-keys', request, {
        timeout: 10000,
      });
      return Ok(response.data);
    } catch (error: any) {
      return this.handleError(error);
    }
  }

  public static async getApiKey(
    id: string,
  ): Promise<Result<ApiKey, ApiKeyServiceError>> {
    try {
      const response = await axios.get(`/api/api-keys/${id}`, {
        timeout: 10000,
      });
      return Ok(response.data);
    } catch (error: any) {
      return this.handleError(error);
    }
  }

  public static async deleteApiKey(
    id: string,
  ): Promise<Result<{ message: string }, ApiKeyServiceError>> {
    try {
      const response = await axios.delete(`/api/api-keys/${id}`, {
        timeout: 10000,
      });
      return Ok(response.data);
    } catch (error: any) {
      return this.handleError(error);
    }
  }

  public static async getApiKeyStats(): Promise<
    Result<ApiKeyStats, ApiKeyServiceError>
  > {
    try {
      const response = await axios.get('/api/api-keys/stats/summary', {
        timeout: 10000,
      });
      return Ok(response.data);
    } catch (error: any) {
      return this.handleError(error);
    }
  }

  private static handleError(error: any): Result<any, ApiKeyServiceError> {
    return Failure({
      message: getErrorMessage(error),
      statusCode: getErrorStatusCode(error),
    });
  }
}
