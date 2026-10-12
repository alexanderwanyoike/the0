import { Injectable } from "@nestjs/common";
import { and, asc, eq, isNull, lt, sql } from "drizzle-orm";
import { PgDatabase, PgQueryResultHKT } from "drizzle-orm/pg-core";
import { getDatabase } from "@/database/connection";
import { botEventOutboxTable } from "@/database/schema/bot-event-outbox";

/** A message the runtime must receive about a change to a Bot. */
export interface BotEvent {
  botId: string;
  subject: string;
  payload: Record<string, unknown>;
}

/** An event in the outbox, with its payload already encoded. */
export interface PendingBotEvent {
  id: number;
  botId: string;
  subject: string;
  payload: string;
}

/**
 * published: one event went out and more may be waiting. drained: nothing
 * was left to publish. failed: the publisher rejected the event.
 */
export type PublishOutcome = "published" | "drained" | "failed";

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

@Injectable()
export class BotEventOutbox {
  /**
   * Passes the oldest unpublished event to `publish` and marks it if
   * accepted, with no transaction or lock held across the publish. Several
   * relays may run at once: each moves on only after JetStream acks, so a
   * bot's later event never goes out before an earlier one is stored, and
   * a repeat carries the same Nats-Msg-Id, which JetStream drops.
   */
  async publishNext(
    publish: (event: PendingBotEvent) => Promise<boolean>,
  ): Promise<PublishOutcome> {
    const db = getDatabase() as Executor;
    const table = botEventOutboxTable;
    const [next] = await db
      .select({
        id: table.id,
        botId: table.botId,
        subject: table.subject,
        payload: table.payload,
      })
      .from(table)
      .where(isNull(table.publishedAt))
      .orderBy(asc(table.id))
      .limit(1);
    if (!next) return "drained";
    if (!(await publish(next))) return "failed";

    await db
      .update(table)
      .set({ publishedAt: sql`now()` })
      .where(and(eq(table.id, next.id), isNull(table.publishedAt)));
    return "published";
  }

  async prunePublishedBefore(cutoff: Date): Promise<number> {
    const db = getDatabase() as Executor;
    const removed = await db
      .delete(botEventOutboxTable)
      .where(lt(botEventOutboxTable.publishedAt, cutoff))
      .returning({ id: botEventOutboxTable.id });
    return removed.length;
  }
}
