export type BotType = "scheduled" | "realtime" | "event";

export const BOT_TYPES: BotType[] = ["scheduled", "realtime", "event"];

export type CustomBotStatus = "active";

export type Runtime = "python3.11" | "nodejs20";

export interface CustomBotConfig {
  name: string;
  description: string;
  version: string;
  type: BotType;
  runtime: Runtime;
  author: string;
  entrypoints: {
    bot: string;
  };
  schema: {
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
  createdAt: Date;
  updatedAt: Date;
}

export interface CustomBotVersion {
  version: string;
  config: CustomBotConfig;
  userId: string;
  id: string;
  filePath: string;
  status: CustomBotStatus;
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
