export type BotType = 'scheduled' | 'realtime' | 'event';

export const BOT_TYPES: BotType[] = ['scheduled', 'realtime', 'event'];

export type CustomBotStatus =
  | 'approved'
  | 'declined'
  | 'awaiting_human_review'
  | 'pending_review'
  | 'published';

export type Runtime = 'python3.11' | 'nodejs20';

export interface CustomBotConfig {
  name: string;
  description: string;
  version: string;
  type: BotType;
  runtime: Runtime;
  author: string;
  entrypoints: {
    bot: string;
    backtest?: string;
  };
  schema: {
    backtest?: Record<string, any>;
    bot: Record<string, any>;
  };
  readme: string;
  metadata?: {
    categories?: string[];
    instruments?: string[];
    exchanges?: string[];
    tags?: string[];
    // Allow additional metadata fields
    [key: string]: any;
  };
}

export interface CustomBot {
  id: string;
  name: string;
  version: string;
  config: CustomBotConfig;
  filePath: string;
  userId: string;
  status: CustomBotStatus;
  review?: any; // Security analysis results from 0vers33r  
  marketplace?: MarketplaceMetadata | null;
  createdAt: Date;
  updatedAt: Date;
}

export interface CustomBotVersion {
  version: string;
  config: CustomBotConfig;
  userId: string;
  id: string;
  marketplace?: MarketplaceMetadata | null;
  filePath: string;
  status: CustomBotStatus;
  review?: any; // Security analysis results from 0vers33r
  createdAt: Date;
  updatedAt: Date;
}

export interface CustomBotWithVersions {
  id: string;
  name: string;
  userId: string;
  versions: CustomBotVersion[];
  latestVersion: string;
  createdAt: Date;
  updatedAt: Date;
}

export interface MarketplaceMetadata {
  isPublished: boolean;
  publishedAt?: Date;
  price: number; // 0 for free
  description: string;
  tags: string[];
  category?: string;
  installCount: number;
  averageRating?: number; // 1-5 scale
  totalReviews: number;
  revenue: number; // Total earnings
  lastUpdated?: Date;
}
