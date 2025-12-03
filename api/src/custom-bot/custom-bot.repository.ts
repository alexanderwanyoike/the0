import { Injectable } from "@nestjs/common";
import { RoleRevisionRepository } from "@/common/role-revision.repository";
import {
  CustomBot,
  CustomBotWithVersions,
  CustomBotVersion,
} from "./custom-bot.types";
import { Result, Ok, Failure } from "@/common/result";
import { eq, and, desc } from "drizzle-orm";
import * as semver from "semver";

@Injectable()
export class CustomBotRepository extends RoleRevisionRepository<CustomBot> {
  protected readonly tableName = "customBots" as const;

  constructor() {
    super("name"); // Use 'name' as the revision key field
  }

  async userBotExists(
    userId: string,
    name: string,
  ): Promise<Result<boolean, string>> {
    try {
      const result = await this.findByKey(userId, name);
      if (!result.success) {
        return Failure(result.error);
      }
      return Ok(result.data.length > 0);
    } catch (error: any) {
      return Failure(error.message);
    }
  }

  async globalBotExists(name: string): Promise<Result<boolean, string>> {
    try {
      const result = await this.findGlobalByKey(name);
      if (!result.success) {
        return Failure(result.error);
      }
      return Ok(result.data.length > 0);
    } catch (error: any) {
      return Failure(error.message);
    }
  }

  async userVersionExists(
    userId: string,
    name: string,
    version: string,
  ): Promise<Result<boolean, string>> {
    try {
      const result = await this.findByKeyAndVersion(userId, name, version);
      return Ok(result.success);
    } catch (error: any) {
      return Failure(error.message);
    }
  }

  async globalVersionExists(
    name: string,
    version: string,
  ): Promise<Result<boolean, string>> {
    try {
      const result = await this.findGlobalByKeyAndVersion(name, version);
      return Ok(result.success);
    } catch (error: any) {
      return Failure(error.message);
    }
  }

  async getUserCustomBots(
    userId: string,
  ): Promise<Result<CustomBotWithVersions[], string>> {
    try {
      // Get all custom bots for the user
      const result = await this.findAll(userId);
      if (!result.success) {
        return Failure(result.error);
      }

      if (result.data.length === 0) {
        return Ok([]);
      }

      // Group bots by name
      const botsByName = new Map<string, CustomBot[]>();
      for (const bot of result.data) {
        if (!botsByName.has(bot.name)) {
          botsByName.set(bot.name, []);
        }
        botsByName.get(bot.name)!.push(bot);
      }

      // Transform each group to CustomBotWithVersions
      const customBotsWithVersions: CustomBotWithVersions[] = [];

      for (const [botName, bots] of botsByName) {
        // Sort by creation date descending (latest first)
        bots.sort(
          (a, b) =>
            new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime(),
        );

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

        const customBotWithVersions: CustomBotWithVersions = {
          id: latestBot.id,
          name: latestBot.name,
          userId: latestBot.userId,
          latestVersion: latestBot.version,
          versions,
          createdAt: bots[bots.length - 1].createdAt, // First created
          updatedAt: latestBot.updatedAt, // Latest updated
        };

        customBotsWithVersions.push(customBotWithVersions);
      }

      // Sort by latest update time descending
      customBotsWithVersions.sort(
        (a, b) =>
          new Date(b.updatedAt).getTime() - new Date(a.updatedAt).getTime(),
      );

      return Ok(customBotsWithVersions);
    } catch (error: any) {
      return Failure(error.message);
    }
  }

  async getAllUserVersions(
    userId: string,
    name: string,
  ): Promise<Result<CustomBotWithVersions, string>> {
    try {
      const result = await this.findByKey(userId, name);
      if (!result.success) {
        return Failure(result.error);
      }

      if (result.data.length === 0) {
        return Failure("Bot not found");
      }

      const bots = result.data;
      const latestBot = bots[0]; // Already ordered by createdAt desc

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

      const customBotWithVersions: CustomBotWithVersions = {
        id: latestBot.id,
        name: latestBot.name,
        userId: latestBot.userId,
        latestVersion: latestBot.version,
        versions,
        createdAt: bots[bots.length - 1].createdAt, // First created
        updatedAt: latestBot.updatedAt, // Latest updated
      };

      return Ok(customBotWithVersions);
    } catch (error: any) {
      return Failure(error.message);
    }
  }

  async getAllGlobalVersions(
    name: string,
  ): Promise<Result<CustomBotWithVersions, string>> {
    try {
      const result = await this.findGlobalByKey(name);
      if (!result.success) {
        return Failure(result.error);
      }

      if (result.data.length === 0) {
        return Failure("Bot not found");
      }

      const bots = result.data;
      const latestBot = bots[0]; // Already ordered by createdAt desc

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

      const customBotWithVersions: CustomBotWithVersions = {
        id: latestBot.id,
        name: latestBot.name,
        userId: latestBot.userId,
        latestVersion: latestBot.version,
        versions,
        createdAt: bots[bots.length - 1].createdAt, // First created
        updatedAt: latestBot.updatedAt, // Latest updated
      };

      return Ok(customBotWithVersions);
    } catch (error: any) {
      return Failure(error.message);
    }
  }

  async getSpecificUserVersion(
    userId: string,
    name: string,
    version: string,
  ): Promise<Result<CustomBot, string>> {
    return this.findByKeyAndVersion(userId, name, version);
  }

  async getSpecificGlobalVersion(
    name: string,
    version: string,
  ): Promise<Result<CustomBot, string>> {
    return this.findGlobalByKeyAndVersion(name, version);
  }

  async createNewGlobalVersion(
    userId: string,
    botData: Partial<CustomBot>,
  ): Promise<Result<CustomBot, string>> {
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
  ): Promise<Result<boolean, string>> {
    const result = await this.findGlobalByKey(name);
    if (!result.success) {
      return Failure(result.error);
    }
    const bots = result.data;
    if (bots.length === 0) {
      return Failure("Bot not found");
    }
    const bot = bots[0];
    if (bot.userId !== userId) {
      return Failure("Insufficient permissions");
    }
    return Ok(true);
  }

  // Legacy aliases for backward compatibility
  async getVersionsForBot(
    userId: string,
    name: string,
  ): Promise<Result<CustomBotVersion[], string>> {
    const result = await this.getAllUserVersions(userId, name);
    if (!result.success) {
      return Failure(result.error);
    }
    return Ok(result.data.versions);
  }

  async getBotsWithVersions(
    userId: string,
  ): Promise<Result<CustomBotWithVersions[], string>> {
    return this.getUserCustomBots(userId);
  }

  async createBot(
    userId: string,
    customBot: Partial<CustomBot>,
  ): Promise<Result<CustomBot, string>> {
    const data = {
      userId,
      name: customBot.name!,
      version: customBot.version!,
      config: customBot.config!,
      filePath: customBot.filePath!,
      status: customBot.status || "active",
    };

    return this.create(data);
  }
}
