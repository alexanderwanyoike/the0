import { Result, Ok, Failure } from './result';
import { getDatabase, getTables } from '@/database/connection';
import { eq, and, desc } from 'drizzle-orm';
import Repository from './repository';
import { createId } from '@paralleldrive/cuid2';

export abstract class RoleRepository<T> implements Repository {
  protected readonly db = getDatabase();
  protected readonly tables = getTables();
  protected abstract readonly tableName: keyof typeof this.tables;

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
      console.log('Error creating document:', error);
      return Failure(error.message);
    }
  }

  async findAll(userId: string): Promise<Result<T[], string>> {
    try {
      console.log('🔍 RoleRepository.findAll called with userId:', userId, 'type:', typeof userId);
      
      if (!userId) {
        console.log('❌ User ID is missing or falsy');
        return Failure('User ID is required');
      }

      console.log('📊 Executing query on table:', this.tableName);
      const records = await this.db.select().from(this.table)
        .where(eq(this.table.userId, userId))
        .orderBy(desc(this.table.createdAt));

      console.log('✅ Query executed successfully, found', records.length, 'records');
      return Ok(records.map(record => this.transformSnapshotToData<T>(record)));
    } catch (error: any) {
      console.log('❌ RoleRepository.findAll - Error fetching documents:', error);
      return Failure(error.message);
    }
  }

  async findOne(userId: string, id: string): Promise<Result<T, string>> {
    try {
      if (!userId || !id) {
        return Failure('User ID and ID are required');
      }

      const records = await this.db.select().from(this.table)
        .where(and(
          eq(this.table.userId, userId),
          eq(this.table.id, id)
        ));

      if (records.length === 0) {
        return Failure('Not found');
      }

      return Ok(this.transformSnapshotToData<T>(records[0]));
    } catch (error: any) {
      console.log('Error fetching document:', error);
      return Failure(error.message);
    }
  }

  async update(userId: string, id: string, args: Partial<T>): Promise<Result<T, string>> {
    try {
      if (!userId || !id) {
        return Failure('User ID and ID are required');
      }

      const updateData = {
        ...args,
        updatedAt: new Date(),
      };

      await this.db.update(this.table)
        .set(updateData)
        .where(and(
          eq(this.table.userId, userId),
          eq(this.table.id, id)
        ));

      return this.findOne(userId, id);
    } catch (error: any) {
      console.log('Error updating document:', error);
      return Failure(error.message);
    }
  }

  async remove(userId: string, id: string): Promise<Result<void, string>> {
    try {
      if (!userId || !id) {
        return Failure('User ID and ID are required');
      }

      await this.db.delete(this.table)
        .where(and(
          eq(this.table.userId, userId),
          eq(this.table.id, id)
        ));

      return Ok(null);
    } catch (error: any) {
      console.log('Error deleting document:', error);
      return Failure(error.message);
    }
  }

  protected transformSnapshotToData<T>(record: any): T {
    return {
      id: record.id,
      ...record,
    } as T;
  }
}