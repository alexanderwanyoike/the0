import { Injectable } from '@nestjs/common';
import { RoleRepository } from '@/common/role.repository';
import { UserBot, UserBotQueryParams } from './user-bots.types';
import { Result, Ok, Failure } from '@/common/result';
import { eq, and, desc } from 'drizzle-orm';

@Injectable()
export class UserBotsRepository extends RoleRepository<UserBot> {
  protected readonly tableName = 'userBots' as const;

  async findByUserId(userId: string, params?: UserBotQueryParams): Promise<Result<UserBot[], string>> {
    return this.findAll(userId);
  }

  async findByCustomBotId(userId: string, customBotId: string): Promise<Result<UserBot[], string>> {
    try {
      const records = await this.db.select().from(this.table)
        .where(and(
          eq(this.table.userId, userId),
          eq(this.table.customBotId, customBotId)
        ))
        .orderBy(desc(this.table.createdAt));

      return Ok(records.map(record => this.transformSnapshotToData<UserBot>(record)));
    } catch (error: any) {
      return Failure(error.message);
    }
  }

  async findByUserIdAndBotName(userId: string, customBotName: string): Promise<Result<UserBot | null, string>> {
    try {
      const records = await this.db.select().from(this.table)
        .where(and(
          eq(this.table.userId, userId),
          eq(this.table.customBotName, customBotName)
        ));

      if (records.length === 0) {
        return Ok(null);
      }

      return Ok(this.transformSnapshotToData<UserBot>(records[0]));
    } catch (error: any) {
      return Failure(error.message);
    }
  }

  async hasUserBot(userId: string, customBotName: string): Promise<Result<boolean, string>> {
    try {
      const result = await this.findByUserIdAndBotName(userId, customBotName);
      if (!result.success) {
        return Failure(result.error);
      }
      return Ok(result.data !== null);
    } catch (error: any) {
      return Failure(error.message);
    }
  }

  async addBotToUser(userId: string, customBotName: string): Promise<Result<UserBot, string>> {
    try {
      // Check if user already has the bot
      const existingResult = await this.findByUserIdAndBotName(userId, customBotName);
      if (!existingResult.success) {
        return Failure(existingResult.error);
      }
      
      if (existingResult.data !== null) {
        return Failure('User already owns this bot');
      }

      // Add the bot to the user
      return await this.create({
        userId,
        customBotName,
        acquiredAt: new Date(),
      });
    } catch (error: any) {
      return Failure(error.message);
    }
  }
}