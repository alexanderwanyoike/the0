import { Result, Ok, Failure, errorMessage } from "./result";
import { getDatabase, getTables } from "@/database/connection";
import { eq, and, desc, asc } from "drizzle-orm";
import { createId } from "@paralleldrive/cuid2";
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

  async create(args: Partial<T>): Promise<Result<T, string>> {
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
      return Failure(errorMessage(error));
    }
  }

  async findAll(userId: string): Promise<Result<T[], string>> {
    try {
      if (!userId) {
        return Failure("User ID is required");
      }

      const records = await this.db
        .select()
        .from(this.table)
        .where(eq(this.table.userId, userId))
        .orderBy(desc(this.table.createdAt));

      return Ok(records.map((record: Record<string, unknown>) => this.transformRecordToData(record)));
    } catch (error: unknown) {
      logger.error({ err: error }, "Error fetching documents");
      return Failure(errorMessage(error));
    }
  }

  async findOne(userId: string, id: string): Promise<Result<T, string>> {
    try {
      const records = await this.db
        .select()
        .from(this.table)
        .where(and(eq(this.table.userId, userId), eq(this.table.id, id)));

      if (records.length === 0) {
        return Failure("Not found");
      }

      return Ok(this.transformRecordToData(records[0]));
    } catch (error: unknown) {
      logger.error({ err: error }, "Error fetching document");
      return Failure(errorMessage(error));
    }
  }

  async findOneById(id: string): Promise<Result<T, string>> {
    try {
      const records = await this.db
        .select()
        .from(this.table)
        .where(eq(this.table.id, id));

      if (records.length === 0) {
        return Failure("Not found");
      }

      return Ok(this.transformRecordToData(records[0]));
    } catch (error: unknown) {
      logger.error({ err: error }, "Error fetching document by ID");
      return Failure(errorMessage(error));
    }
  }

  async update(
    userId: string,
    id: string,
    args: Partial<T>,
  ): Promise<Result<T, string>> {
    try {
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
      return Failure(errorMessage(error));
    }
  }

  async remove(userId: string, id: string): Promise<Result<void, string>> {
    try {
      await this.db
        .delete(this.table)
        .where(and(eq(this.table.userId, userId), eq(this.table.id, id)));

      return Ok(null);
    } catch (error: unknown) {
      logger.error({ err: error }, "Error deleting document");
      return Failure(errorMessage(error));
    }
  }

  // Generic revision methods using configurable key field
  async findByKey(userId: string, key: string): Promise<Result<T[], string>> {
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

      return Ok(records.map((record: Record<string, unknown>) => this.transformRecordToData(record)));
    } catch (error: unknown) {
      logger.error(
        { err: error, keyField: this.keyField },
        "Error fetching documents by key",
      );
      return Failure(errorMessage(error));
    }
  }

  async findByKeyAndVersion(
    userId: string,
    key: string,
    version: string,
  ): Promise<Result<T, string>> {
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
        return Failure("Not found");
      }

      return Ok(this.transformRecordToData(records[0]));
    } catch (error: unknown) {
      logger.error(
        { err: error, keyField: this.keyField },
        "Error fetching document by key and version",
      );
      return Failure(errorMessage(error));
    }
  }

  async getLatestVersion(
    userId: string,
    keyValue: string,
  ): Promise<Result<T, string>> {
    try {
      const records = await this.db
        .select()
        .from(this.table)
        .where(
          and(
            eq(this.table.userId, userId),
            eq(this.table[this.keyField], keyValue),
          ),
        )
        .orderBy(desc(this.table.createdAt))
        .limit(1);

      if (records.length === 0) {
        return Failure("Not found");
      }

      return Ok(this.transformRecordToData(records[0]));
    } catch (error: unknown) {
      logger.error({ err: error }, "Error fetching latest version");
      return Failure(errorMessage(error));
    }
  }

  // Global methods (not user-scoped) for checking uniqueness
  async getGlobalLatestVersion(key: string): Promise<Result<T, string>> {
    try {
      const records = await this.db
        .select()
        .from(this.table)
        .where(eq(this.table[this.keyField], key))
        .orderBy(desc(this.table.createdAt))
        .limit(1);

      if (records.length === 0) {
        return Failure("Not found");
      }

      return Ok(this.transformRecordToData(records[0]));
    } catch (error: unknown) {
      logger.error({ err: error }, "Error fetching global latest version");
      return Failure(errorMessage(error));
    }
  }

  async findGlobalByKey(key: string): Promise<Result<T[], string>> {
    try {
      const records = await this.db
        .select()
        .from(this.table)
        .where(eq(this.table[this.keyField], key))
        .orderBy(desc(this.table.createdAt));

      return Ok(records.map((record: Record<string, unknown>) => this.transformRecordToData(record)));
    } catch (error: unknown) {
      logger.error(
        { err: error, keyField: this.keyField },
        "Error fetching global documents by key",
      );
      return Failure(errorMessage(error));
    }
  }

  async globalKeyExists(key: string): Promise<Result<boolean, string>> {
    try {
      const result = await this.findGlobalByKey(key);
      if (!result.success) {
        return Failure(result.error);
      }
      return Ok(result.data.length > 0);
    } catch (error: unknown) {
      return Failure(errorMessage(error));
    }
  }

  async findGlobalByKeyAndVersion(
    key: string,
    version: string,
  ): Promise<Result<T, string>> {
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
        return Failure("Not found");
      }

      return Ok(this.transformRecordToData(records[0]));
    } catch (error: unknown) {
      logger.error(
        { err: error, keyField: this.keyField },
        "Error fetching global document by key and version",
      );
      return Failure(errorMessage(error));
    }
  }

  protected transformRecordToData(record: Record<string, unknown>): T {
    return {
      id: record.id,
      ...record,
    } as T;
  }

  // Legacy method name for backward compatibility
  protected transformSnapshotToData<U>(record: Record<string, unknown>): U {
    return this.transformRecordToData(record) as unknown as U;
  }
}
