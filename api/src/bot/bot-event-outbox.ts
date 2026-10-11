import { Injectable } from "@nestjs/common";
import { asc, inArray, isNull, lt, sql } from "drizzle-orm";
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
 * drained: nothing was left to publish. more: a full batch went out and
 * more may be waiting. failed: the publisher rejected an event. busy:
 * another relay holds the lock.
 */
export type PublishOutcome = "drained" | "more" | "failed" | "busy";

/** A database, or a transaction already open on one. */
export type Executor = PgDatabase<PgQueryResultHKT>;

// Any fixed key works, as long as no other advisory lock on this database uses it.
const RELAY_LOCK_KEY = 4810917311;

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
   * Passes up to `limit` unpublished events to `publish` in id order and
   * marks the ones it accepts. Stops at the first rejection, so no event
   * overtakes an earlier one.
   */
  async publishPending(
    publish: (event: PendingBotEvent) => Promise<boolean>,
    limit: number,
  ): Promise<PublishOutcome> {
    const db = getDatabase() as Executor;
    const table = botEventOutboxTable;
    return db.transaction(async (tx) => {
      // Transaction-level, because Neon's pooler runs in transaction mode,
      // where a session-level lock would not stay with this connection.
      const [{ locked }] = await tx
        .select({
          locked: sql<boolean>`pg_try_advisory_xact_lock(${sql.raw(String(RELAY_LOCK_KEY))})`,
        })
        .from(sql`(SELECT 1) AS relay_lock`);
      if (!locked) return "busy";

      const pending = await tx
        .select({
          id: table.id,
          botId: table.botId,
          subject: table.subject,
          payload: table.payload,
        })
        .from(table)
        .where(isNull(table.publishedAt))
        .orderBy(asc(table.id))
        .limit(limit);

      const accepted: number[] = [];
      let outcome: PublishOutcome =
        pending.length === limit ? "more" : "drained";
      for (const event of pending) {
        if (!(await publish(event))) {
          outcome = "failed";
          break;
        }
        accepted.push(event.id);
      }

      if (accepted.length > 0) {
        await tx
          .update(table)
          .set({ publishedAt: new Date() })
          .where(inArray(table.id, accepted));
      }
      return outcome;
    });
  }

  /** Deletes events published before `cutoff`, returning how many. */
  async prunePublishedBefore(cutoff: Date): Promise<number> {
    const db = getDatabase() as Executor;
    const removed = await db
      .delete(botEventOutboxTable)
      .where(lt(botEventOutboxTable.publishedAt, cutoff))
      .returning({ id: botEventOutboxTable.id });
    return removed.length;
  }
}
