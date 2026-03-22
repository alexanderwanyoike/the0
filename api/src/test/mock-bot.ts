import { Bot } from "../bot/entities/bot.entity";
import { BotConfig } from "../database/schema/bots";

const defaultConfig: BotConfig = {
  name: "test-bot",
  type: "custom/test-bot",
  version: "1.0.0",
  foo: "string-value",
  bar: 42,
};

const defaults: Bot = {
  id: "test-bot-id",
  name: "test-bot",
  userId: "test-user-id",
  customBotId: "test-custom-bot-id",
  topic: "the0-scheduled-custom-bot",
  config: defaultConfig,
  createdAt: new Date("2024-01-01"),
  updatedAt: new Date("2024-01-01"),
};

export function mockBot(overrides: Partial<Bot> = {}): Bot {
  return { ...defaults, ...overrides };
}

export function mockBotConfig(overrides: Partial<BotConfig> = {}): BotConfig {
  return { ...defaultConfig, ...overrides };
}
