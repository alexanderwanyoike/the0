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
export const BOT_LOG_TOPICS = {
  PREFIX: "the0.bot.logs",
  forBot(botId: string) {
    return `${this.PREFIX}.${botId}`;
  },
} as const;
