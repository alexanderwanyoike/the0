import { UserBot } from './entities/user-bot.entity';
import { CustomBotWithVersions } from '@/custom-bot/custom-bot.types';

export interface UserBotSummary {
  id: string;
  customBotName: string;
  acquiredAt: Date;
  customBot?: CustomBotWithVersions;
}

export interface UserBotQueryParams {
  limit?: number;
  offset?: number;
}

export { UserBot };
