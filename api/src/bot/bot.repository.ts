import { Injectable } from "@nestjs/common";
import { and, asc, eq } from "drizzle-orm";
import pino from "pino";
import { Bot } from "./entities/bot.entity";
import { RoleRepository } from "@/common/role.repository";
import { appError, AppError, WRITE_FAILED } from "@/common/app-error";
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

  /**
   * Disables every enabled bot the user owns and stores their events, all
   * or none: a bot whose events are refused rolls every change back and
   * returns the refusal. The rows are locked as they are read, so a
   * concurrent update is not overwritten with the config read here.
   */
  async disableAllWithEvents(
    userId: string,
    eventsFor: (disabled: Bot) => Result<BotEvent[], AppError>,
  ): Promise<Result<DisableAllOutcome, AppError>> {
    // An object, not a let: TypeScript would narrow a let assigned only
    // inside the transaction callback to null in the catch below.
    const refusal: { error?: AppError } = {};
    try {
      const outcome = await this.db.transaction(async (tx: Executor) => {
        const rows = (await tx
          .select()
          .from(this.table)
          .where(eq(this.table.userId, userId))
          .orderBy(asc(this.table.name))
          .for("update")) as Record<string, unknown>[];
        const bots = rows.map((row) => this.transformSnapshotToData<Bot>(row));

        const disabled: Bot[] = [];
        for (const bot of bots.filter(isEnabled)) {
          const [updated] = (await tx
            .update(this.table)
            .set({
              config: { ...bot.config, enabled: false },
              updatedAt: new Date(),
            })
            .where(this.ownedBy(userId, bot.id))
            .returning()) as Record<string, unknown>[];
          disabled.push(this.transformSnapshotToData<Bot>(updated));
        }
        const events: BotEvent[] = [];
        for (const bot of disabled) {
          const built = eventsFor(bot);
          if (!built.success) {
            refusal.error = built.error;
            // Throwing is how a drizzle transaction rolls back.
            throw new Error(`Events refused for bot ${bot.id}`);
          }
          events.push(...built.data);
        }
        await appendBotEvents(tx, events);
        return {
          disabled,
          alreadyDisabled: bots.filter((bot) => !isEnabled(bot)),
          events,
        };
      });
      return Ok(outcome);
    } catch (error: unknown) {
      if (refusal.error) return Failure(refusal.error);
      logger.error({ err: error }, "Error disabling bots");
      return Failure(WRITE_FAILED);
    }
  }

  private ownedBy(userId: string, id: string) {
    return and(eq(this.table.userId, userId), eq(this.table.id, id));
  }
}

/** What disableAllWithEvents changed, and the events it stored for the relay. */
export interface DisableAllOutcome {
  disabled: Bot[];
  alreadyDisabled: Bot[];
  events: BotEvent[];
}

/** The runtime's rule (desired.IsEnabled): a bot runs unless config.enabled is false. */
const isEnabled = (bot: Bot): boolean => bot.config?.enabled !== false;

/** customBotId is the only reference a caller can break: by deleting the version. */
const writeFailure = (error: unknown): AppError =>
  isForeignKeyViolation(error)
    ? appError("validation", "Custom bot version no longer exists")
    : WRITE_FAILED;
