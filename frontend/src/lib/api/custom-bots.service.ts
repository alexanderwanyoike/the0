import { ApiClient, ApiResponse } from '@/lib/api-client';
import { Result, Ok, Failure } from '@/lib/result';
import { CustomBotWithVersions, CustomBotVersion, CustomBotStatus, CustomBotConfig } from '@/types/custom-bots';

export interface CustomBotServiceError {
  message: string;
  statusCode: number;
}

export class CustomBotService {
  /**
   * Get all custom bots for the current user with version grouping logic
   * This replaces the Firestore onSnapshot listener in use-custom-bots.ts
   */
  static async getCustomBots(): Promise<Result<CustomBotWithVersions[], CustomBotServiceError>> {
    try {
      const response = await ApiClient.get<{success: boolean, data: any[], message: string}>('/api/custom-bots');
      
      if (!response.data || !response.data.success) {
        return Failure({
          message: response.error?.message || 'Failed to fetch custom bots',
          statusCode: response.error?.statusCode || 500,
        });
      }

      // Check if data is already in CustomBotWithVersions format (has versions array)
      const rawData = response.data.data;
      let customBots: CustomBotWithVersions[];
      
      if (rawData.length > 0 && rawData[0].versions) {
        // Data is already transformed by the backend, just convert dates
        customBots = rawData.map((bot: any) => ({
          ...bot,
          createdAt: new Date(bot.createdAt),
          updatedAt: new Date(bot.updatedAt),
          versions: bot.versions.map((version: any) => ({
            ...version,
            createdAt: new Date(version.createdAt),
            review: version.review
              ? {
                  ...version.review,
                  reviewedAt: version.review.reviewedAt ? new Date(version.review.reviewedAt) : undefined,
                }
              : undefined,
          })),
        }));
      } else {
        // Data needs transformation (fallback for raw individual records)
        customBots = this.transformToCustomBotsWithVersions(rawData);
      }

      return Ok(customBots);
    } catch (error: any) {
      return Failure({
        message: error.message || 'Network error',
        statusCode: 500,
      });
    }
  }

  /**
   * Get a specific custom bot by name with all versions
   * This replaces the Firestore listener in use-custom-bot.ts
   */
  static async getCustomBot(botName: string): Promise<Result<CustomBotWithVersions, CustomBotServiceError>> {
    try {
      // First get all custom bots to find the one with the matching name
      const allBotsResult = await this.getCustomBots();
      
      if (!allBotsResult.success) {
        return Failure(allBotsResult.error);
      }

      const customBot = allBotsResult.data.find(bot => bot.name === botName);
      
      if (!customBot) {
        return Failure({
          message: 'Bot not found',
          statusCode: 404,
        });
      }

      return Ok(customBot);
    } catch (error: any) {
      return Failure({
        message: error.message || 'Network error',
        statusCode: 500,
      });
    }
  }

  /**
   * Transform raw API response to CustomBotWithVersions format
   * This preserves the complex version grouping logic from the original Firestore hooks
   */
  private static transformToCustomBotsWithVersions(rawBots: any[]): CustomBotWithVersions[] {
    const botData: CustomBotWithVersions[] = [];
    const botsByName = new Map<string, any[]>();

    // Safety check - ensure rawBots is actually an array
    if (!Array.isArray(rawBots)) {
      throw new Error(`Expected array but got ${typeof rawBots}`);
    }

    // Group bots by name to handle versions (same logic as original hook)
    rawBots.forEach((rawBot) => {
      const bot = {
        id: rawBot.id,
        ...rawBot,
        // Convert ISO string dates to Date objects for UI compatibility
        createdAt: new Date(rawBot.createdAt),
        updatedAt: new Date(rawBot.updatedAt),
        review: rawBot.review
          ? {
              ...rawBot.review,
              reviewedAt: rawBot.review.reviewedAt ? new Date(rawBot.review.reviewedAt) : undefined,
            }
          : undefined,
      };

      if (!botsByName.has(bot.name)) {
        botsByName.set(bot.name, []);
      }
      botsByName.get(bot.name)!.push(bot);
    });

    // Transform to CustomBotWithVersions format (preserving original logic)
    for (const [botName, bots] of botsByName) {
      bots.sort((a, b) => b.createdAt.getTime() - a.createdAt.getTime());
      const latestBot = bots[0];

      const versions: CustomBotVersion[] = bots.map((bot) => ({
        id: bot.id,
        version: bot.version,
        userId: bot.userId,
        createdAt: bot.createdAt,
        status: bot.status,
        config: bot.config,
        filePath: bot.filePath,
        review: bot.review,
      }));

      const customBotWithVersions: CustomBotWithVersions = {
        id: latestBot.id,
        name: latestBot.name,
        userId: latestBot.userId,
        latestVersion: latestBot.version,
        versions,
        createdAt: bots[bots.length - 1].createdAt, // First created
        updatedAt: latestBot.updatedAt, // Latest updated
      };

      botData.push(customBotWithVersions);
    }

    return botData;
  }

  /**
   * Get specific version of a custom bot
   */
  static async getCustomBotVersion(botName: string, version: string): Promise<Result<any, CustomBotServiceError>> {
    try {
      const response = await ApiClient.get<any>(`/api/custom-bots/${encodeURIComponent(botName)}/${encodeURIComponent(version)}`);
      
      if (!response.data) {
        return Failure({
          message: response.error?.message || 'Failed to fetch custom bot version',
          statusCode: response.error?.statusCode || 500,
        });
      }

      // Convert date strings to Date objects for compatibility
      const botData = {
        ...response.data,
        createdAt: new Date(response.data.createdAt),
        updatedAt: new Date(response.data.updatedAt),
        review: response.data.review
          ? {
              ...response.data.review,
              reviewedAt: response.data.review.reviewedAt ? new Date(response.data.review.reviewedAt) : undefined,
            }
          : undefined,
      };

      return Ok(botData);
    } catch (error: any) {
      return Failure({
        message: error.message || 'Network error',
        statusCode: 500,
      });
    }
  }

  /**
   * Get all versions for a specific custom bot
   */
  static async getCustomBotVersions(botName: string): Promise<Result<string[], CustomBotServiceError>> {
    try {
      const response = await ApiClient.get<string[]>(`/api/custom-bots/${encodeURIComponent(botName)}/versions`);
      
      if (!response.data) {
        return Failure({
          message: response.error?.message || 'Failed to fetch custom bot versions',
          statusCode: response.error?.statusCode || 500,
        });
      }

      return Ok(response.data);
    } catch (error: any) {
      return Failure({
        message: error.message || 'Network error',
        statusCode: 500,
      });
    }
  }
}