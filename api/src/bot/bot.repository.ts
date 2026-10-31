import { Injectable } from "@nestjs/common";
import { and, eq } from "drizzle-orm";
import pino from "pino";
import { Bot } from "./entities/bot.entity";
import { RoleRepository, WRITE_FAILED } from "@/common/role.repository";
import { appError, AppError } from "@/common/app-error";
import { isForeignKeyViolation } from "@/common/database-errors";
import { Failure, Ok, Result } from "@/common/result";
import { appendBotEvents, BotEvent, Executor } from "./bot-event-outbox";

const logger = pino({ name: "BotRepository" });

@Injectable()
export class BotRepository extends RoleRepository<Bot> {
  protected readonly tableName = "bots" as const;

  constructor() {
    super();
  }

  async createWithEvents(
    bot: Partial<Bot>,
    events: BotEvent[],
  ): Promise<Result<Bot, AppError>> {
    try {
      const now = new Date();
      const record = await this.db.transaction(async (tx: Executor) => {
        const [inserted] = (await tx
          .insert(this.table)
          .values({ ...bot, createdAt: now, updatedAt: now })
          .returning()) as Record<string, unknown>[];
        await appendBotEvents(tx, events);
        return inserted;
      });
      return Ok(this.transformSnapshotToData<Bot>(record));
    } catch (error: unknown) {
      logger.error({ err: error }, "Error creating bot");
      return Failure(writeFailure(error));
    }
  }

  /** Fails with "Not found", and stores no event, unless the user owns the bot. */
  async updateWithEvents(
    userId: string,
    id: string,
    changes: Partial<Bot>,
    events: BotEvent[],
  ): Promise<Result<Bot, AppError>> {
    try {
      const updated: boolean = await this.db.transaction(
        async (tx: Executor) => {
          const rows = await tx
            .update(this.table)
            .set({ ...changes, updatedAt: new Date() })
            .where(this.ownedBy(userId, id))
            .returning({ id: this.table.id });
          if (rows.length === 0) return false;
          await appendBotEvents(tx, events);
          return true;
        },
      );
      if (!updated) return Failure(appError("not_found", "Not found"));
      return this.findOne(userId, id);
    } catch (error: unknown) {
      logger.error({ err: error }, "Error updating bot");
      return Failure(writeFailure(error));
    }
  }

  /** Stores the events only if a bot was deleted. */
  async removeWithEvents(
    userId: string,
    id: string,
    events: BotEvent[],
  ): Promise<Result<void, AppError>> {
    try {
      await this.db.transaction(async (tx: Executor) => {
        const rows = await tx
          .delete(this.table)
          .where(this.ownedBy(userId, id))
          .returning({ id: this.table.id });
        if (rows.length > 0) await appendBotEvents(tx, events);
      });
      return Ok(null);
    } catch (error: unknown) {
      logger.error({ err: error }, "Error deleting bot");
      return Failure(WRITE_FAILED);
    }
  }

  private ownedBy(userId: string, id: string) {
    return and(eq(this.table.userId, userId), eq(this.table.id, id));
  }
}

/** customBotId is the only reference a caller can break: by deleting the version. */
const writeFailure = (error: unknown): AppError =>
  isForeignKeyViolation(error)
    ? appError("validation", "Custom bot version no longer exists")
    : WRITE_FAILED;
