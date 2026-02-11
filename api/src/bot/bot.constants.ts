export const BOT_TYPE_PATTERN =
  /^[a-z0-9]+(-[a-z0-9]+)*\/[a-z0-9]+(-[a-z0-9]+)*$/;

// NATS topics for bot events (match runtime expectations)
export const BOT_TOPICS = {
  CREATED: "the0.bot.created",
  UPDATED: "the0.bot.updated",
  DELETED: "the0.bot.deleted",
} as const;

// NATS topics for scheduled bot events
export const SCHEDULED_BOT_TOPICS = {
  CREATED: "the0.bot-schedule.created",
  UPDATED: "the0.bot-schedule.updated",
  DELETED: "the0.bot-schedule.deleted",
} as const;

// NATS topics for bot log streaming
const BOT_LOG_PREFIX = "the0.bot.logs";

export const BOT_LOG_TOPICS = {
  PREFIX: BOT_LOG_PREFIX,
  forBot(botId: string) {
    return `${BOT_LOG_PREFIX}.${botId}`;
  },
} as const;
