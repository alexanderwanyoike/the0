import { BotSchema } from "@/lib/api/api-client";

export type CustomBotStatus = "active";

export type BotType = "scheduled" | "realtime" | "event";
export type Runtime = "python3.11" | "nodejs20";

// Custom Bot Configuration
export interface CustomBotConfig {
  name: string;
  version: string;
  description: string;
  runtime: Runtime;
  type: BotType;
  author: string;
  entrypoints: {
    bot: string;
    [key: string]: string;
  };
  schema: {
    [key: string]: BotSchema;
  };
  metadata?: {
    [key: string]: any;
  };
  readme?: string;
}

// Individual Version of a Custom Bot
export interface CustomBotVersion {
  id: string;
  version: string;
  userId: string;
  createdAt: Date;
  status: CustomBotStatus;
  config: CustomBotConfig;
  filePath: string;
}

// Custom Bot with All Versions
export interface CustomBotWithVersions {
  id: string;
  name: string;
  userId: string;
  latestVersion: string;
  versions: CustomBotVersion[];
  createdAt: Date;
  updatedAt: Date;
}

// Single Custom Bot Document
export interface CustomBot {
  id: string;
  name: string;
  version: string;
  userId: string;
  status: CustomBotStatus;
  config: CustomBotConfig;
  filePath: string;
  createdAt: Date;
  updatedAt: Date;
}
