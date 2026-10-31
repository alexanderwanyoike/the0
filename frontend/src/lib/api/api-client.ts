import { ApiError, apiRequest } from "@/lib/auth-fetch";
import { Ok, Result } from "@/lib/result";

export interface BotSchema {
  properties: Record<string, any>;
  required: string[];
  type: string;
}

export interface Bot {
  id: string;
  config: Record<string, any>;
  status?: string;
  createdAt: string;
  updatedAt: string;
}

export type BotServiceError = ApiError;

const TIMEOUT_MS = 120_000;

export class BotService {
  public static getBot(id: string): Promise<Result<Bot, BotServiceError>> {
    return apiRequest(`/api/bot/${id}`, { timeoutMs: TIMEOUT_MS });
  }

  public static updateBot(
    id: string,
    config: Record<any, any>,
  ): Promise<Result<Bot, BotServiceError>> {
    return apiRequest(`/api/bot/${id}`, {
      method: "PUT",
      body: { name: config.name, config },
      timeoutMs: TIMEOUT_MS,
    });
  }

  public static getBots(): Promise<Result<Bot[], BotServiceError>> {
    return apiRequest("/api/bot", { timeoutMs: TIMEOUT_MS });
  }

  public static async deleteBot(
    id: string,
  ): Promise<Result<null, BotServiceError>> {
    const result = await apiRequest(`/api/bot/${id}`, {
      method: "DELETE",
      timeoutMs: TIMEOUT_MS,
    });
    return result.success ? Ok(null) : result;
  }
}
