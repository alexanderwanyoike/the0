import { appError, AppError, READ_FAILED, WRITE_FAILED } from "./app-error";
import { Result, Ok, Failure } from "./result";
import { getDatabase, getTables } from "@/database/connection";
import { eq, and, desc } from "drizzle-orm";
import Repository from "./repository";
import pino from "pino";

const logger = pino({ name: "RoleRepository" });

export abstract class RoleRepository<T> implements Repository<T> {
  protected readonly db = getDatabase();
  protected readonly tables = getTables();
  protected abstract readonly tableName: keyof typeof this.tables;

  // eslint-disable-next-line @typescript-eslint/no-explicit-any -- dynamic table access via computed key
  protected get table(): any {
    return this.tables[this.tableName];
  }

  async create(args: Partial<T>): Promise<Result<T, AppError>> {
    try {
      const data = {
        ...args,
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      const records = await this.db.insert(this.table).values(data).returning();

      return Ok({
        id: records[0].id,
        ...data,
      } as T);
    } catch (error: unknown) {
      logger.error({ err: error }, "Error creating document");
      return Failure(WRITE_FAILED);
    }
  }

  async findAll(userId: string): Promise<Result<T[], AppError>> {
    try {
      if (!userId) {
        return Failure(appError("validation", "User ID is required"));
      }

      const records = await this.db
        .select()
        .from(this.table)
        .where(eq(this.table.userId, userId))
        .orderBy(desc(this.table.createdAt));

      return Ok(
        records.map((record: Record<string, unknown>) =>
          this.transformSnapshotToData<T>(record),
        ),
      );
    } catch (error: unknown) {
      logger.error({ err: error }, "Error fetching documents");
      return Failure(READ_FAILED);
    }
  }

  async findOne(userId: string, id: string): Promise<Result<T, AppError>> {
    try {
      if (!userId || !id) {
        return Failure(appError("validation", "User ID and ID are required"));
      }

      const records = await this.db
        .select()
        .from(this.table)
        .where(and(eq(this.table.userId, userId), eq(this.table.id, id)));

      if (records.length === 0) {
        return Failure(appError("not_found", "Not found"));
      }

      return Ok(this.transformSnapshotToData<T>(records[0]));
    } catch (error: unknown) {
      logger.error({ err: error }, "Error fetching document");
      return Failure(READ_FAILED);
    }
  }

  async update(
    userId: string,
    id: string,
    args: Partial<T>,
  ): Promise<Result<T, AppError>> {
    try {
      if (!userId || !id) {
        return Failure(appError("validation", "User ID and ID are required"));
      }

      const updateData = {
        ...args,
        updatedAt: new Date(),
      };

      await this.db
        .update(this.table)
        .set(updateData)
        .where(and(eq(this.table.userId, userId), eq(this.table.id, id)));

      return this.findOne(userId, id);
    } catch (error: unknown) {
      logger.error({ err: error }, "Error updating document");
      return Failure(WRITE_FAILED);
    }
  }

  async remove(userId: string, id: string): Promise<Result<void, AppError>> {
    try {
      if (!userId || !id) {
        return Failure(appError("validation", "User ID and ID are required"));
      }

      await this.db
        .delete(this.table)
        .where(and(eq(this.table.userId, userId), eq(this.table.id, id)));

      return Ok(null);
    } catch (error: unknown) {
      logger.error({ err: error }, "Error deleting document");
      return Failure(WRITE_FAILED);
    }
  }

  protected transformSnapshotToData<U>(record: Record<string, unknown>): U {
    return {
      id: record.id,
      ...record,
    } as U;
  }
}
