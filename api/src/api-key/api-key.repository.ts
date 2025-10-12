import { Injectable } from "@nestjs/common";
import { RoleRepository } from "@/common/role.repository";
import { ApiKey } from "./models/api-key.model";
import { Result, Ok, Failure } from "@/common/result";
import { eq, and } from "drizzle-orm";
import { createId } from "@paralleldrive/cuid2";
import * as crypto from "crypto";

@Injectable()
export class ApiKeyRepository extends RoleRepository<ApiKey> {
  protected readonly tableName = "apiKeys" as const;

  async createApiKey(
    userId: string,
    name: string,
  ): Promise<Result<ApiKey, string>> {
    try {
      // Generate a secure API key
      const apiKey = this.generateApiKey();

      const data = {
        userId,
        name,
        keyValue: apiKey,
        isActive: true,
      };

      const result = await this.create(data);

      if (!result.success) {
        return Failure(result.error);
      }

      // Return with the actual key
      return Ok({
        ...result.data,
        key: apiKey,
      } as ApiKey);
    } catch (error: any) {
      return Failure(error.message);
    }
  }

  async findByKey(key: string): Promise<Result<ApiKey, string>> {
    try {
      const records = await this.db
        .select()
        .from(this.table)
        .where(
          and(eq(this.table.keyValue, key), eq(this.table.isActive, true)),
        );

      if (records.length === 0) {
        return Failure("API key not found or inactive");
      }

      const apiKey = records[0];

      return Ok({
        id: apiKey.id,
        userId: apiKey.userId,
        name: apiKey.name,
        key: apiKey.keyValue,
        isActive: apiKey.isActive,
        createdAt: apiKey.createdAt,
        updatedAt: apiKey.createdAt,
        lastUsedAt: apiKey.lastUsedAt,
      } as ApiKey);
    } catch (error: any) {
      return Failure(error.message);
    }
  }

  async updateLastUsed(keyId: string): Promise<Result<void, string>> {
    try {
      await this.db
        .update(this.table)
        .set({ lastUsedAt: new Date() })
        .where(eq(this.table.id, keyId));

      return Ok(null);
    } catch (error: any) {
      return Failure(error.message);
    }
  }

  async deleteApiKey(
    userId: string,
    id: string,
  ): Promise<Result<void, string>> {
    try {
      console.log("🔑 Deleting API key:", { userId, id });

      // First check if the API key exists and belongs to the user
      const existingKey = await this.findOne(userId, id);
      if (!existingKey.success) {
        console.log("❌ API key not found:", existingKey.error);
        return Failure("API key not found");
      }

      console.log("✅ Found API key to delete:", existingKey.data.name);

      await this.db
        .delete(this.table)
        .where(and(eq(this.table.id, id), eq(this.table.userId, userId)));

      console.log("✅ API key deleted successfully");
      return Ok(null);
    } catch (error: any) {
      console.log("❌ Error deleting API key:", error);
      return Failure(error.message);
    }
  }

  // Override transformSnapshotToData to show full API key info
  protected transformSnapshotToData<T>(record: any): T {
    return {
      id: record.id,
      userId: record.userId,
      name: record.name,
      key: record.keyValue,
      isActive: record.isActive,
      createdAt: record.createdAt,
      updatedAt: record.updatedAt,
      lastUsedAt: record.lastUsedAt,
    } as T;
  }

  private generateApiKey(): string {
    const prefix = "the0_";
    const randomBytes = crypto.randomBytes(32).toString("hex");
    return `${prefix}${randomBytes}`;
  }
}
