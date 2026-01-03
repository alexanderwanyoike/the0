import { Injectable, Scope } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { PinoLogger } from "nestjs-pino";
import { BotService } from "@/bot/bot.service";
import { Result, Ok, Failure } from "@/common/result";
import * as Minio from "minio";
import * as tar from "tar";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";

export interface StateKey {
  key: string;
  size?: number;
}

@Injectable({ scope: Scope.REQUEST })
export class BotStateService {
  private minioClient: Minio.Client;
  private stateBucket: string;

  constructor(
    private readonly configService: ConfigService,
    private readonly botService: BotService,
    private readonly logger: PinoLogger,
  ) {
    this.minioClient = new Minio.Client({
      endPoint:
        this.configService.get<string>("MINIO_ENDPOINT") || "localhost",
      port: parseInt(this.configService.get<string>("MINIO_PORT") || "9000"),
      useSSL: this.configService.get<string>("MINIO_USE_SSL") === "true",
      accessKey:
        this.configService.get<string>("MINIO_ACCESS_KEY") || "minioadmin",
      secretKey:
        this.configService.get<string>("MINIO_SECRET_KEY") || "minioadmin",
    });
    this.stateBucket =
      this.configService.get<string>("MINIO_STATE_BUCKET") || "bot-state";
  }

  /**
   * List all state keys for a bot.
   */
  async listKeys(botId: string): Promise<Result<StateKey[], string>> {
    // Verify bot ownership
    const botResult = await this.botService.findOne(botId);
    if (!botResult.success) {
      return Failure("Bot not found or access denied");
    }

    try {
      const tempDir = await this.downloadAndExtractState(botId);
      if (!tempDir) {
        return Ok([]); // No state exists yet
      }

      try {
        const stateDir = path.join(tempDir, ".the0-state");
        if (!fs.existsSync(stateDir)) {
          return Ok([]);
        }

        const files = fs.readdirSync(stateDir);
        const keys: StateKey[] = files
          .filter((f) => f.endsWith(".json"))
          .map((f) => {
            const filepath = path.join(stateDir, f);
            const stats = fs.statSync(filepath);
            return {
              key: f.slice(0, -5), // Remove .json extension
              size: stats.size,
            };
          });

        return Ok(keys);
      } finally {
        // Cleanup temp directory
        fs.rmSync(tempDir, { recursive: true, force: true });
      }
    } catch (error: any) {
      this.logger.error({ err: error, botId }, "Error listing state keys");
      return Failure(`Failed to list state keys: ${error.message}`);
    }
  }

  /**
   * Get a specific state value.
   */
  async getKey(
    botId: string,
    key: string,
  ): Promise<Result<unknown, string>> {
    // Verify bot ownership
    const botResult = await this.botService.findOne(botId);
    if (!botResult.success) {
      return Failure("Bot not found or access denied");
    }

    // Validate key
    if (!key || key.includes("/") || key.includes("\\") || key.includes("..")) {
      return Failure("Invalid state key");
    }

    try {
      const tempDir = await this.downloadAndExtractState(botId);
      if (!tempDir) {
        return Failure("State key not found");
      }

      try {
        const filepath = path.join(tempDir, ".the0-state", `${key}.json`);
        if (!fs.existsSync(filepath)) {
          return Failure("State key not found");
        }

        const content = fs.readFileSync(filepath, "utf-8");
        const value = JSON.parse(content);
        return Ok(value);
      } finally {
        // Cleanup temp directory
        fs.rmSync(tempDir, { recursive: true, force: true });
      }
    } catch (error: any) {
      this.logger.error({ err: error, botId, key }, "Error getting state key");
      return Failure(`Failed to get state key: ${error.message}`);
    }
  }

  /**
   * Delete a specific state key.
   */
  async deleteKey(botId: string, key: string): Promise<Result<boolean, string>> {
    // Verify bot ownership
    const botResult = await this.botService.findOne(botId);
    if (!botResult.success) {
      return Failure("Bot not found or access denied");
    }

    // Validate key
    if (!key || key.includes("/") || key.includes("\\") || key.includes("..")) {
      return Failure("Invalid state key");
    }

    try {
      const tempDir = await this.downloadAndExtractState(botId);
      if (!tempDir) {
        return Ok(false); // No state exists
      }

      try {
        const filepath = path.join(tempDir, ".the0-state", `${key}.json`);
        if (!fs.existsSync(filepath)) {
          return Ok(false);
        }

        // Delete the file
        fs.unlinkSync(filepath);

        // Re-upload the modified state
        await this.uploadState(botId, tempDir);

        return Ok(true);
      } finally {
        // Cleanup temp directory
        fs.rmSync(tempDir, { recursive: true, force: true });
      }
    } catch (error: any) {
      this.logger.error({ err: error, botId, key }, "Error deleting state key");
      return Failure(`Failed to delete state key: ${error.message}`);
    }
  }

  /**
   * Clear all state for a bot.
   */
  async clearState(botId: string): Promise<Result<boolean, string>> {
    // Verify bot ownership
    const botResult = await this.botService.findOne(botId);
    if (!botResult.success) {
      return Failure("Bot not found or access denied");
    }

    try {
      const statePath = `${botId}/state.tar.gz`;

      // Check if state exists
      try {
        await this.minioClient.statObject(this.stateBucket, statePath);
      } catch (error: any) {
        if (error.code === "NotFound") {
          return Ok(true); // No state to clear
        }
        throw error;
      }

      // Delete the state archive
      await this.minioClient.removeObject(this.stateBucket, statePath);

      return Ok(true);
    } catch (error: any) {
      this.logger.error({ err: error, botId }, "Error clearing state");
      return Failure(`Failed to clear state: ${error.message}`);
    }
  }

  /**
   * Download and extract bot state to a temporary directory.
   * Returns null if no state exists.
   */
  private async downloadAndExtractState(
    botId: string,
  ): Promise<string | null> {
    const statePath = `${botId}/state.tar.gz`;

    // Check if state exists
    try {
      await this.minioClient.statObject(this.stateBucket, statePath);
    } catch (error: any) {
      if (error.code === "NotFound") {
        return null;
      }
      throw error;
    }

    // Create temp directory
    const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), "bot-state-"));
    const tarPath = path.join(tempDir, "state.tar.gz");

    try {
      // Download tar.gz
      await this.minioClient.fGetObject(this.stateBucket, statePath, tarPath);

      // Extract tar.gz
      await tar.x({
        file: tarPath,
        cwd: tempDir,
      });

      // Remove the tar.gz file
      fs.unlinkSync(tarPath);

      return tempDir;
    } catch (error) {
      // Cleanup on error
      fs.rmSync(tempDir, { recursive: true, force: true });
      throw error;
    }
  }

  /**
   * Upload state from a temporary directory back to MinIO.
   */
  private async uploadState(botId: string, tempDir: string): Promise<void> {
    const statePath = `${botId}/state.tar.gz`;
    const tarPath = path.join(tempDir, "state.tar.gz");
    const stateDir = path.join(tempDir, ".the0-state");

    // Check if there's any state left
    if (!fs.existsSync(stateDir)) {
      // No state directory, remove the object
      try {
        await this.minioClient.removeObject(this.stateBucket, statePath);
      } catch {
        // Ignore errors when removing
      }
      return;
    }

    const files = fs.readdirSync(stateDir).filter((f) => f.endsWith(".json"));
    if (files.length === 0) {
      // No state files left, remove the object
      try {
        await this.minioClient.removeObject(this.stateBucket, statePath);
      } catch {
        // Ignore errors when removing
      }
      return;
    }

    // Create tar.gz from .the0-state directory
    await tar.c(
      {
        gzip: true,
        file: tarPath,
        cwd: tempDir,
      },
      [".the0-state"],
    );

    // Upload to MinIO
    await this.minioClient.fPutObject(this.stateBucket, statePath, tarPath);
  }
}
