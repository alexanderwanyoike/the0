import { Result, Ok, Failure } from "./result";
import { getDatabase, getTables } from "@/database/connection";
import { eq, and, desc, asc } from "drizzle-orm";
import { createId } from "@paralleldrive/cuid2";

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
    } catch (error: any) {
      console.log("Error creating document:", error);
      return Failure(error.message);
    }
  }

  async findAll(userId: string): Promise<Result<T[], string>> {
    try {
      console.log(
        "🔍 RoleRevisionRepository.findAll called with userId:",
        userId,
        "type:",
        typeof userId,
      );

      if (!userId) {
        console.log("❌ User ID is missing or falsy in RoleRevisionRepository");
        return Failure("User ID is required");
      }

      console.log("📊 Executing query on revision table:", this.tableName);
      const records = await this.db
        .select()
        .from(this.table)
        .where(eq(this.table.userId, userId))
        .orderBy(desc(this.table.createdAt));

      console.log(
        "✅ Revision query executed successfully, found",
        records.length,
        "records",
      );
      return Ok(records.map((record) => this.transformRecordToData(record)));
    } catch (error: any) {
      console.log(
        "❌ RoleRevisionRepository.findAll - Error fetching documents:",
        error,
      );
      return Failure(error.message);
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
    } catch (error: any) {
      console.log("Error fetching document:", error);
      return Failure(error.message);
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
    } catch (error: any) {
      console.log("Error updating document:", error);
      return Failure(error.message);
    }
  }

  async remove(userId: string, id: string): Promise<Result<void, string>> {
    try {
      await this.db
        .delete(this.table)
        .where(and(eq(this.table.userId, userId), eq(this.table.id, id)));

      return Ok(null);
    } catch (error: any) {
      console.log("Error deleting document:", error);
      return Failure(error.message);
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

      return Ok(records.map((record) => this.transformRecordToData(record)));
    } catch (error: any) {
      console.log(`Error fetching documents by ${this.keyField}:`, error);
      return Failure(error.message);
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
    } catch (error: any) {
      console.log(
        `Error fetching document by ${this.keyField} and version:`,
        error,
      );
      return Failure(error.message);
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
    } catch (error: any) {
      console.log("Error fetching latest version:", error);
      return Failure(error.message);
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
    } catch (error: any) {
      console.log("Error fetching global latest version:", error);
      return Failure(error.message);
    }
  }

  async findGlobalByKey(key: string): Promise<Result<T[], string>> {
    try {
      const records = await this.db
        .select()
        .from(this.table)
        .where(eq(this.table[this.keyField], key))
        .orderBy(desc(this.table.createdAt));

      return Ok(records.map((record) => this.transformRecordToData(record)));
    } catch (error: any) {
      console.log(
        `Error fetching global documents by ${this.keyField}:`,
        error,
      );
      return Failure(error.message);
    }
  }

  async globalKeyExists(key: string): Promise<Result<boolean, string>> {
    try {
      const result = await this.findGlobalByKey(key);
      if (!result.success) {
        return Failure(result.error);
      }
      return Ok(result.data.length > 0);
    } catch (error: any) {
      return Failure(error.message);
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
    } catch (error: any) {
      console.log(
        `Error fetching global document by ${this.keyField} and version:`,
        error,
      );
      return Failure(error.message);
    }
  }

  protected transformRecordToData(record: any): T {
    return {
      id: record.id,
      ...record,
    } as T;
  }

  // Legacy method name for backward compatibility
  protected transformSnapshotToData<T>(record: any): T {
    return this.transformRecordToData(record) as unknown as T;
  }
}
