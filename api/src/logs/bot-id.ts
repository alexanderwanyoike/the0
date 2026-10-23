// Must stay in sync with runtime's sanitizeBotID (runtime/internal/daemon/nats_publisher.go)
const VALID_BOT_ID = /^[A-Za-z0-9_-]+$/;

export const isValidBotId = (botId: string): boolean =>
  VALID_BOT_ID.test(botId);
