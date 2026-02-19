import { Bot } from "@/lib/api/api-client";

/**
 * Determines whether a bot should use SSE log streaming or REST polling.
 *
 * Realtime bots have always-running containers that publish logs to NATS,
 * so SSE streaming works. Scheduled bots only run during brief cron windows,
 * so polling is more appropriate.
 *
 * Returns true for realtime/unknown bots (SSE with 4s fallback to polling).
 * Returns false for scheduled bots (direct polling).
 */
export function shouldUseLogStreaming(bot: Bot | null): boolean {
  if (!bot) return false;

  const botType = bot.config?.type;
  if (typeof botType !== "string") return true;

  const [category] = botType.split("/");
  return category !== "scheduled";
}
