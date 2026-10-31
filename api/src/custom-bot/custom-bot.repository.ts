import { Injectable } from "@nestjs/common";
import { RoleRevisionRepository } from "@/common/role-revision.repository";
import {
  CustomBot,
  CustomBotWithVersions,
  CustomBotVersion,
} from "./custom-bot.types";
import { appError, AppError } from "@/common/app-error";
import { Result, Ok, Failure, errorMessage } from "@/common/result";
import { isForeignKeyViolation } from "@/common/database-errors";
import { eq, and, desc, inArray, type SQL } from "drizzle-orm";
import * as semver from "semver";

export type RemoveVersionsFailure =
  | { reason: "in-use" }
  | { reason: "error"; message: string };

@Injectable()
export class CustomBotRepository extends RoleRevisionRepository<CustomBot> {
  protected readonly tableName = "customBots" as const;

  constructor() {
    super("name"); // Use 'name' as the revision key field
  }

  async userBotExists(
    userId: string,
    name: string,
  ): Promise<Result<boolean, AppError>> {
    try {
      const result = await this.findByKey(userId, name);
      if (!result.success) {
        return Failure(result.error);
      }
      return Ok(result.data.length > 0);
    } catch (error: unknown) {
      return Failure(appError("internal", errorMessage(error)));
    }
  }

  async globalBotExists(name: string): Promise<Result<boolean, AppError>> {
    try {
      const result = await this.findGlobalByKey(name);
      if (!result.success) {
        return Failure(result.error);
      }
      return Ok(result.data.length > 0);
    } catch (error: unknown) {
      return Failure(appError("internal", errorMessage(error)));
    }
  }

  async userVersionExists(
    userId: string,
    name: string,
    version: string,
  ): Promise<Result<boolean, AppError>> {
    try {
      const result = await this.findByKeyAndVersion(userId, name, version);
      return Ok(result.success);
    } catch (error: unknown) {
      return Failure(appError("internal", errorMessage(error)));
    }
  }

  async globalVersionExists(
    name: string,
    version: string,
  ): Promise<Result<boolean, AppError>> {
    try {
      const result = await this.findGlobalByKeyAndVersion(name, version);
      return Ok(result.success);
    } catch (error: unknown) {
      return Failure(appError("internal", errorMessage(error)));
    }
  }

  async getUserCustomBots(
    userId: string,
  ): Promise<Result<CustomBotWithVersions[], AppError>> {
    try {
      // Get all custom bots for the user
      const result = await this.findAll(userId);
      if (!result.success) {
        return Failure(result.error);
      }

      if (result.data.length === 0) {
        return Ok([]);
      }

      return Ok(groupByName(result.data));
    } catch (error: unknown) {
      return Failure(appError("internal", errorMessage(error)));
    }
  }

  async getAllUserVersions(
    userId: string,
    name: string,
  ): Promise<Result<CustomBotWithVersions, AppError>> {
    try {
      const result = await this.findByKey(userId, name);
      if (!result.success) {
        return Failure(result.error);
      }

      if (result.data.length === 0) {
        return Failure(appError("not_found", "Bot not found"));
      }

      // findByKey already orders by createdAt desc.
      return Ok(toBotWithVersions(result.data));
    } catch (error: unknown) {
      return Failure(appError("internal", errorMessage(error)));
    }
  }

  async getAllGlobalVersions(
    name: string,
  ): Promise<Result<CustomBotWithVersions, AppError>> {
    try {
      const result = await this.findGlobalByKey(name);
      if (!result.success) {
        return Failure(result.error);
      }

      if (result.data.length === 0) {
        return Failure(appError("not_found", "Bot not found"));
      }

      // findGlobalByKey already orders by createdAt desc.
      return Ok(toBotWithVersions(result.data));
    } catch (error: unknown) {
      return Failure(appError("internal", errorMessage(error)));
    }
  }

  async getSpecificUserVersion(
    userId: string,
    name: string,
    version: string,
  ): Promise<Result<CustomBot, AppError>> {
    return this.findByKeyAndVersion(userId, name, version);
  }

  async getSpecificGlobalVersion(
    name: string,
    version: string,
  ): Promise<Result<CustomBot, AppError>> {
    return this.findGlobalByKeyAndVersion(name, version);
  }

  async createNewGlobalVersion(
    userId: string,
    botData: Partial<CustomBot>,
  ): Promise<Result<CustomBot, AppError>> {
    return this.create({
      ...botData,
      userId,
    });
  }

  isVersionNewer(currentVersion: string, newVersion: string): boolean {
    return semver.gt(newVersion, currentVersion);
  }

  async checkUserOwnership(
    userId: string,
    name: string,
  ): Promise<Result<boolean, AppError>> {
    const result = await this.findGlobalByKey(name);
    if (!result.success) {
      return Failure(result.error);
    }
    const bots = result.data;
    if (bots.length === 0) {
      return Failure(appError("not_found", "Bot not found"));
    }
    const bot = bots[0];
    if (bot.userId !== userId) {
      return Failure(appError("forbidden", "Insufficient permissions"));
    }
    return Ok(true);
  }

  async countInstancesByCustomBotId(
    customBotId: string,
  ): Promise<Result<number, AppError>> {
    try {
      const records = await this.db
        .select()
        .from(this.tables.bots)
        .where(eq(this.tables.bots.customBotId, customBotId));

      return Ok(records.length);
    } catch (error: unknown) {
      return Failure(appError("internal", errorMessage(error)));
    }
  }

  async countInstancesByCustomBotIds(
    ids: string[],
  ): Promise<Result<Map<string, number>, AppError>> {
    try {
      if (ids.length === 0) {
        return Ok(new Map());
      }

      const records = await this.db
        .select()
        .from(this.tables.bots)
        .where(inArray(this.tables.bots.customBotId, ids));

      const counts = new Map<string, number>();
      for (const id of ids) {
        counts.set(id, 0);
      }
      for (const record of records) {
        const id = record.customBotId;
        counts.set(id, (counts.get(id) || 0) + 1);
      }

      return Ok(counts);
    } catch (error: unknown) {
      return Failure(appError("internal", errorMessage(error)));
    }
  }

  async removeVersion(
    userId: string,
    id: string,
  ): Promise<Result<void, RemoveVersionsFailure>> {
    return this.removeWhere(
      and(eq(this.table.userId, userId), eq(this.table.id, id)),
    );
  }

  async removeAllByName(
    userId: string,
    name: string,
  ): Promise<Result<void, RemoveVersionsFailure>> {
    return this.removeWhere(
      and(eq(this.table.userId, userId), eq(this.table[this.keyField], name)),
    );
  }

  private async removeWhere(
    condition: SQL | undefined,
  ): Promise<Result<void, RemoveVersionsFailure>> {
    try {
      await this.db.delete(this.table).where(condition);
      return Ok(null);
    } catch (error: unknown) {
      if (isForeignKeyViolation(error)) {
        return Failure({ reason: "in-use" });
      }
      return Failure({ reason: "error", message: errorMessage(error) });
    }
  }

  async getAllGlobalCustomBots(): Promise<
    Result<CustomBotWithVersions[], AppError>
  > {
    try {
      // Get all custom bots from the database
      const records = await this.db
        .select()
        .from(this.table)
        .orderBy(desc(this.table.createdAt));

      if (records.length === 0) {
        return Ok([]);
      }

      return Ok(
        groupByName(
          records.map((record: Record<string, unknown>) =>
            this.transformRecordToData(record),
          ),
        ),
      );
    } catch (error: unknown) {
      return Failure(appError("internal", errorMessage(error)));
    }
  }
}

// `bots` are versions of one custom bot, newest first.
function toBotWithVersions(bots: CustomBot[]): CustomBotWithVersions {
  const latestBot = bots[0];
  const versions: CustomBotVersion[] = bots.map((bot) => ({
    id: bot.id,
    version: bot.version,
    config: bot.config,
    userId: bot.userId,
    filePath: bot.filePath,
    status: bot.status,
    createdAt: bot.createdAt,
    updatedAt: bot.updatedAt,
  }));

  return {
    id: latestBot.id,
    name: latestBot.name,
    userId: latestBot.userId,
    latestVersion: latestBot.version,
    versions,
    createdAt: bots[bots.length - 1].createdAt,
    updatedAt: latestBot.updatedAt,
  };
}

function groupByName(bots: CustomBot[]): CustomBotWithVersions[] {
  const botsByName = new Map<string, CustomBot[]>();
  for (const bot of bots) {
    if (!botsByName.has(bot.name)) {
      botsByName.set(bot.name, []);
    }
    botsByName.get(bot.name)!.push(bot);
  }

  return [...botsByName.values()]
    .map((versions) =>
      toBotWithVersions(
        versions.sort(
          (a, b) =>
            new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime(),
        ),
      ),
    )
    .sort(
      (a, b) =>
        new Date(b.updatedAt).getTime() - new Date(a.updatedAt).getTime(),
    );
}
