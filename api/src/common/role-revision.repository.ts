import { appError, AppError, READ_FAILED, WRITE_FAILED } from "./app-error";
import { Result, Ok, Failure } from "./result";
import { getDatabase, getTables } from "@/database/connection";
import { eq, and, desc } from "drizzle-orm";
import pino from "pino";

const logger = pino({ name: "RoleRevisionRepository" });

export interface RevisionEntity {
  id?: string;
  userId: string;
  createdAt: Date;
  updatedAt: Date;
}

export abstract class RoleRevisionRepository<T extends RevisionEntity> {
  protected readonly db = getDatabase();
  protected readonly tables = getTables();
  protected abstract readonly tableName: keyof typeof this.tables;
  protected keyField: string;

  constructor(revisionKeyField = "name") {
    this.keyField = revisionKeyField;
  }

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
          this.transformRecordToData(record),
        ),
      );
    } catch (error: unknown) {
      logger.error({ err: error }, "Error fetching documents");
      return Failure(READ_FAILED);
    }
  }

  async findOneById(id: string): Promise<Result<T, AppError>> {
    try {
      const records = await this.db
        .select()
        .from(this.table)
        .where(eq(this.table.id, id));

      if (records.length === 0) {
        return Failure(appError("not_found", "Not found"));
      }

      return Ok(this.transformRecordToData(records[0]));
    } catch (error: unknown) {
      logger.error({ err: error }, "Error fetching document by ID");
      return Failure(READ_FAILED);
    }
  }

  async remove(userId: string, id: string): Promise<Result<void, AppError>> {
    try {
      await this.db
        .delete(this.table)
        .where(and(eq(this.table.userId, userId), eq(this.table.id, id)));

      return Ok(null);
    } catch (error: unknown) {
      logger.error({ err: error }, "Error deleting document");
      return Failure(WRITE_FAILED);
    }
  }

  // Generic revision methods using configurable key field
  async findByKey(userId: string, key: string): Promise<Result<T[], AppError>> {
    try {
      const records = await this.db
        .select()
        .from(this.table)
        .where(
          and(
            eq(this.table.userId, userId),
            eq(this.table[this.keyField], key),
          ),
        )
        .orderBy(desc(this.table.createdAt));

      return Ok(
        records.map((record: Record<string, unknown>) =>
          this.transformRecordToData(record),
        ),
      );
    } catch (error: unknown) {
      logger.error(
        { err: error, keyField: this.keyField },
        "Error fetching documents by key",
      );
      return Failure(READ_FAILED);
    }
  }

  async findByKeyAndVersion(
    userId: string,
    key: string,
    version: string,
  ): Promise<Result<T, AppError>> {
    try {
      const records = await this.db
        .select()
        .from(this.table)
        .where(
          and(
            eq(this.table.userId, userId),
            eq(this.table[this.keyField], key),
            eq(this.table.version, version),
          ),
        );

      if (records.length === 0) {
        return Failure(appError("not_found", "Not found"));
      }

      return Ok(this.transformRecordToData(records[0]));
    } catch (error: unknown) {
      logger.error(
        { err: error, keyField: this.keyField },
        "Error fetching document by key and version",
      );
      return Failure(READ_FAILED);
    }
  }

  // Global methods (not user-scoped) for checking uniqueness
  async getGlobalLatestVersion(key: string): Promise<Result<T, AppError>> {
    try {
      const records = await this.db
        .select()
        .from(this.table)
        .where(eq(this.table[this.keyField], key))
        .orderBy(desc(this.table.createdAt))
        .limit(1);

      if (records.length === 0) {
        return Failure(appError("not_found", "Not found"));
      }

      return Ok(this.transformRecordToData(records[0]));
    } catch (error: unknown) {
      logger.error({ err: error }, "Error fetching global latest version");
      return Failure(READ_FAILED);
    }
  }

  async findGlobalByKey(key: string): Promise<Result<T[], AppError>> {
    try {
      const records = await this.db
        .select()
        .from(this.table)
        .where(eq(this.table[this.keyField], key))
        .orderBy(desc(this.table.createdAt));

      return Ok(
        records.map((record: Record<string, unknown>) =>
          this.transformRecordToData(record),
        ),
      );
    } catch (error: unknown) {
      logger.error(
        { err: error, keyField: this.keyField },
        "Error fetching global documents by key",
      );
      return Failure(READ_FAILED);
    }
  }

  async findGlobalByKeyAndVersion(
    key: string,
    version: string,
  ): Promise<Result<T, AppError>> {
    try {
      const records = await this.db
        .select()
        .from(this.table)
        .where(
          and(
            eq(this.table[this.keyField], key),
            eq(this.table.version, version),
          ),
        );

      if (records.length === 0) {
        return Failure(appError("not_found", "Not found"));
      }

      return Ok(this.transformRecordToData(records[0]));
    } catch (error: unknown) {
      logger.error(
        { err: error, keyField: this.keyField },
        "Error fetching global document by key and version",
      );
      return Failure(READ_FAILED);
    }
  }

  protected transformRecordToData(record: Record<string, unknown>): T {
    return {
      id: record.id,
      ...record,
    } as T;
  }
}
