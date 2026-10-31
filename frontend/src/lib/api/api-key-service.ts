import { ApiError, apiRequest } from "@/lib/auth-fetch";
import { Result } from "@/lib/result";

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

export type ApiKeyServiceError = ApiError;

const TIMEOUT_MS = 10_000;

export class ApiKeyService {
  public static getApiKeys(): Promise<Result<ApiKey[], ApiKeyServiceError>> {
    return apiRequest("/api/api-keys", { timeoutMs: TIMEOUT_MS });
  }

  public static createApiKey(
    request: CreateApiKeyRequest,
  ): Promise<Result<ApiKey, ApiKeyServiceError>> {
    return apiRequest("/api/api-keys", {
      method: "POST",
      body: request,
      timeoutMs: TIMEOUT_MS,
    });
  }

  public static getApiKey(
    id: string,
  ): Promise<Result<ApiKey, ApiKeyServiceError>> {
    return apiRequest(`/api/api-keys/${id}`, { timeoutMs: TIMEOUT_MS });
  }

  public static deleteApiKey(
    id: string,
  ): Promise<Result<{ message: string }, ApiKeyServiceError>> {
    return apiRequest(`/api/api-keys/${id}`, {
      method: "DELETE",
      timeoutMs: TIMEOUT_MS,
    });
  }

  public static getApiKeyStats(): Promise<
    Result<ApiKeyStats, ApiKeyServiceError>
  > {
    return apiRequest("/api/api-keys/stats/summary", {
      timeoutMs: TIMEOUT_MS,
    });
  }
}
