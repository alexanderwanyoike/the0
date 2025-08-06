import axios from "@/lib/axios-interceptor";
import { Result, Ok, Failure } from "@/lib/result";
import { getErrorMessage, getErrorStatusCode } from "@/lib/axios";

export interface BotSchema {
  properties: Record<string, any>;
  required: string[];
  type: string;
}

export interface Bot {
  id: string;
  config: Record<string, any>;
  status: string;
  createdAt: string;
  updatedAt: string;
}

export type BotServiceError = {
  message: string;
  statusCode: number;
};

export type BotMeta = {
  name: string;
  exchange: string;
  description: string;
  longDescription: string;
  version: string;
};

export class BotService {
  public static async createBot(
    config: Record<string, any>,
  ): Promise<Result<Bot, BotServiceError>> {
    try {
      const response = await axios.post("/api/bot", {
        name: config.name,
        config,
      });
      return Ok(response.data);
    } catch (error: any) {
      return this.handleError(error);
    }
  }

  public static async getBot(
    id: string,
  ): Promise<Result<Bot, BotServiceError>> {
    try {
      const response = await axios.get(`/api/bot/${id}`, {
        timeout: 120000,
      });
      return Ok(response.data);
    } catch (error: any) {
      return this.handleError(error);
    }
  }

  public static async updateBot(
    id: string,
    config: Record<any, any>,
  ): Promise<Result<Bot, BotServiceError>> {
    try {
      const response = await axios.put(
        `/api/bot/${id}`,
        {
          name: config.name,
          config,
        },
        {
          timeout: 120000,
        },
      );
      return Ok(response.data);
    } catch (error: any) {
      return this.handleError(error);
    }
  }

  public static async getBots(): Promise<Result<Bot[], BotServiceError>> {
    try {
      const response = await axios.get("/api/bot", {
        timeout: 120000,
      });
      return Ok(response.data);
    } catch (error: any) {
      return this.handleError(error);
    }
  }

  public static async deleteBot(
    id: string,
  ): Promise<Result<null, BotServiceError>> {
    try {
      await axios.delete(`/api/bot/${id}`, {
        timeout: 120000,
      });
      return Ok(null);
    } catch (error: any) {
      return this.handleError(error);
    }
  }

  private static handleError(error: any): Result<any, BotServiceError> {
    return Failure({
      message: getErrorMessage(error),
      statusCode: getErrorStatusCode(error),
    });
  }
}
