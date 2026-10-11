import { PgDatabase, PgQueryResultHKT } from "drizzle-orm/pg-core";
import { botEventOutboxTable } from "@/database/schema/bot-event-outbox";

/** A message the runtime must receive about a change to a Bot. */
export interface BotEvent {
  botId: string;
  subject: string;
  payload: Record<string, unknown>;
}

/** A database, or a transaction already open on one. */
export type Executor = PgDatabase<PgQueryResultHKT>;

/** Stores events as part of the caller's transaction. */
export async function appendBotEvents(
  tx: Executor,
  events: BotEvent[],
): Promise<void> {
  if (events.length === 0) return;
  await tx.insert(botEventOutboxTable).values(
    events.map(({ botId, subject, payload }) => ({
      botId,
      subject,
      payload: JSON.stringify(payload),
    })),
  );
}
