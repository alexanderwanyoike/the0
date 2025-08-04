import { Injectable, Inject, Scope } from '@nestjs/common';
import { REQUEST } from '@nestjs/core';
import { UserBotsRepository } from './user-bots.repository';
import { Result, Ok, Failure } from '@/common/result';
import { UserBotSummary, UserBotQueryParams } from './user-bots.types';
import { CustomBotService } from '@/custom-bot/custom-bot.service';

@Injectable({ scope: Scope.REQUEST })
export class UserBotsService {
  constructor(
    @Inject(REQUEST) private readonly request: any,
    private readonly userBotsRepository: UserBotsRepository,
    private readonly customBotService: CustomBotService,
  ) {}

  async getUserBots(
    params?: UserBotQueryParams,
  ): Promise<Result<UserBotSummary[], string>> {
    try {
      const { uid } = this.request.user;

      const result = await this.userBotsRepository.findByUserId(uid, params);

      if (!result.success) {
        return Failure(result.error);
      }

      const summaries: UserBotSummary[] = await Promise.all(
        result.data.map(async (userBot) => {
          // Fetch custom bot information
          const customBotResult =
            await this.customBotService.getAllGlobalVersions(
              userBot.customBotName,
            );

          return {
            id: userBot.id,
            customBotName: userBot.customBotName,
            acquiredAt: userBot.acquiredAt as Date,
            customBot: customBotResult.success
              ? customBotResult.data
              : undefined,
          };
        }),
      );

      return Ok(summaries);
    } catch (error: any) {
      console.error('Error fetching user bots:', error);
      return Failure(`Failed to fetch user bots: ${error.message}`);
    }
  }

  async install(
    customBotName: string,
  ): Promise<Result<UserBotSummary, string>> {
    try {
      const { uid } = this.request.user;

      // Get the bot information
      const botResult = await this.customBotService.getAllGlobalVersions(
        customBotName,
      );
      if (!botResult.success) {
        return Failure('Failed to fetch bot information');
      }

      const customBotVersions = botResult.data;

      if (customBotVersions.versions.length === 0) {
        return Failure('Bot not found or not published');
      }

      const customBot = customBotVersions.versions.find(
        (version) => version.status === 'published' && version.marketplace,
      );

      if (!customBot?.marketplace) {
        return Failure('Bot marketplace information not available');
      }

      // Verify the bot is free
      if (customBot?.marketplace?.price !== 0) {
        return Failure('Bot is not free');
      }

      // Check if user already owns this bot
      const ownershipResult = await this.userBotsRepository.hasUserBot(
        uid,
        customBotName,
      );

      if (!ownershipResult.success) {
        return Failure('Failed to check bot ownership');
      }

      if (ownershipResult.data) {
        return Failure('You already own this bot');
      }

      // Add bot to user's collection
      const addBotResult = await this.userBotsRepository.addBotToUser(
        uid,
        customBotName,
      );

      if (!addBotResult.success) {
        return Failure(
          `Failed to add bot to collection: ${addBotResult.error}`,
        );
      }


      const summary: UserBotSummary = {
        id: addBotResult.data.id,
        customBotName: addBotResult.data.customBotName,
        acquiredAt: addBotResult.data.acquiredAt as Date,
      };

      return Ok(summary);
    } catch (error: any) {
      console.error('Error purchasing free bot:', error);
      return Failure(`Failed to purchase free bot: ${error.message}`);
    }
  }

  async hasUserBot(
    userId: string,
    customBotName: string,
  ): Promise<Result<boolean, string>> {
    try {
      return await this.userBotsRepository.hasUserBot(userId, customBotName);
    } catch (error: any) {
      console.error('Error checking user bot ownership:', error);
      return Failure(`Failed to check bot ownership: ${error.message}`);
    }
  }

  async getCurrentUserHasBot(
    customBotName: string,
  ): Promise<Result<boolean, string>> {
    try {
      const { uid } = this.request.user;
      return await this.hasUserBot(uid, customBotName);
    } catch (error: any) {
      console.error('Error checking current user bot ownership:', error);
      return Failure(`Failed to check bot ownership: ${error.message}`);
    }
  }
}
