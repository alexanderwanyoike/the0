import { Bot } from "@/lib/api/api-client";

/**
 * Determines whether a bot should use SSE log streaming.
 *
 * True for every loaded bot: scheduled runs publish through the same
 * NATS -> SSE pipeline as realtime bots, so streaming pushes their output
 * live the moment a run executes. REST polling exists only as an automatic
 * fallback when the SSE connection fails (handled inside useBotLogs).
 */
export function shouldUseLogStreaming(bot: Bot | null): boolean {
  return !!bot;
}

/**
 * Whether a bot runs on a cron schedule rather than continuously.
 * Drives UI defaults (interval picker starts in "latest" mode because a
 * scheduled bot's most recent output is rarely inside today's window),
 * not the transport - see shouldUseLogStreaming.
 */
export function isScheduledBot(bot: Bot | null): boolean {
  if (!bot) return false;

  const botType = bot.config.type;
  if (typeof botType !== "string") return false;

  const [category] = botType.split("/");
  return category === "scheduled";
}
