import { Injectable, Scope, Inject } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { PinoLogger } from "nestjs-pino";
import { BotService } from "@/bot/bot.service";
import { Result, Ok, Failure } from "@/common/result";
import { MINIO_CLIENT } from "@/minio";
import * as Minio from "minio";
import * as tar from "tar";
import * as fs from "fs/promises";
import * as path from "path";
import * as os from "os";

export enum BotStateErrorCode {
  BOT_NOT_FOUND = "BOT_NOT_FOUND",
  KEY_NOT_FOUND = "KEY_NOT_FOUND",
  INVALID_KEY = "INVALID_KEY",
  STORAGE_ERROR = "STORAGE_ERROR",
  FILE_TOO_LARGE = "FILE_TOO_LARGE",
  INVALID_JSON = "INVALID_JSON",
  CONCURRENT_MODIFICATION = "CONCURRENT_MODIFICATION",
}

/** Default maximum state file size (10MB) - configurable via MAX_STATE_FILE_SIZE_MB env var */
const DEFAULT_MAX_STATE_FILE_SIZE = 10 * 1024 * 1024;

/** Default maximum state archive size (100MB) - prevents DoS from large downloads */
const DEFAULT_MAX_STATE_ARCHIVE_SIZE = 100 * 1024 * 1024;

/** Allowlist regex for state keys: alphanumeric, dash, underscore, 1-255 chars */
const VALID_KEY_REGEX = /^[a-zA-Z0-9_-]{1,255}$/;

/** Allowlist regex for bot IDs: prevents path traversal */
const VALID_BOT_ID_REGEX = /^[a-zA-Z0-9_-]+$/;

export interface BotStateError {
  code: BotStateErrorCode;
  message: string;
}

export interface StateKey {
  key: string;
  size?: number;
}

/** Result of downloading state with ETag for optimistic locking */
interface StateDownloadResult {
  tempDir: string;
  etag: string;
}

@Injectable({ scope: Scope.REQUEST })
export class BotStateService {
  private stateBucket: string;
  private maxStateFileSize: number;
  private maxStateArchiveSize: number;

  constructor(
    @Inject(MINIO_CLIENT) private readonly minioClient: Minio.Client,
    private readonly configService: ConfigService,
    private readonly botService: BotService,
    private readonly logger: PinoLogger,
  ) {
    this.stateBucket =
      this.configService.get<string>("MINIO_STATE_BUCKET") || "bot-state";

    // Load max file size from config (in MB), default to 10MB
    const maxFileSizeMB = parseInt(
      this.configService.get<string>("MAX_STATE_FILE_SIZE_MB") || "10",
    );
    this.maxStateFileSize = maxFileSizeMB * 1024 * 1024;

    // Load max archive size from config (in MB), default to 100MB
    const maxArchiveSizeMB = parseInt(
      this.configService.get<string>("MAX_STATE_ARCHIVE_SIZE_MB") || "100",
    );
    this.maxStateArchiveSize = maxArchiveSizeMB * 1024 * 1024;
  }

  /**
   * Validate state key using allowlist regex.
   * Keys must be 1-255 characters, alphanumeric with dashes and underscores only.
   */
  private isValidKey(key: string): boolean {
    return VALID_KEY_REGEX.test(key);
  }

  /**
   * Validate bot ID to prevent path traversal attacks.
   */
  private isValidBotId(botId: string): boolean {
    return VALID_BOT_ID_REGEX.test(botId);
  }

  /**
   * List all state keys for a bot.
   */
  async listKeys(botId: string): Promise<Result<StateKey[], BotStateError>> {
    // Validate bot ID to prevent path traversal
    if (!this.isValidBotId(botId)) {
      return Failure({
        code: BotStateErrorCode.STORAGE_ERROR,
        message: "Invalid bot ID format",
      });
    }

    // Verify bot ownership
    const botResult = await this.botService.findOne(botId);
    if (!botResult.success) {
      return Failure({
        code: BotStateErrorCode.BOT_NOT_FOUND,
        message: "Bot not found or access denied",
      });
    }

    try {
      const tempDir = await this.downloadAndExtractState(botId);
      if (!tempDir) {
        return Ok([]); // No state exists yet
      }

      try {
        const stateDir = path.join(tempDir, ".the0-state");
        try {
          await fs.access(stateDir);
        } catch {
          return Ok([]);
        }

        const files = await fs.readdir(stateDir);
        const keys: StateKey[] = [];
        for (const f of files.filter((f) => f.endsWith(".json"))) {
          const filepath = path.join(stateDir, f);
          const stats = await fs.stat(filepath);
          keys.push({
            key: f.slice(0, -5), // Remove .json extension
            size: stats.size,
          });
        }

        return Ok(keys);
      } finally {
        // Cleanup temp directory
        await fs.rm(tempDir, { recursive: true, force: true });
      }
    } catch (error: any) {
      this.logger.error({ err: error, botId }, "Error listing state keys");
      return Failure({
        code: BotStateErrorCode.STORAGE_ERROR,
        message: "Failed to list state keys",
      });
    }
  }

  /**
   * Get a specific state value.
   */
  async getKey(
    botId: string,
    key: string,
  ): Promise<Result<unknown, BotStateError>> {
    // Validate bot ID to prevent path traversal
    if (!this.isValidBotId(botId)) {
      return Failure({
        code: BotStateErrorCode.STORAGE_ERROR,
        message: "Invalid bot ID format",
      });
    }

    // Verify bot ownership
    const botResult = await this.botService.findOne(botId);
    if (!botResult.success) {
      return Failure({
        code: BotStateErrorCode.BOT_NOT_FOUND,
        message: "Bot not found or access denied",
      });
    }

    // Validate key using allowlist regex
    if (!this.isValidKey(key)) {
      return Failure({
        code: BotStateErrorCode.INVALID_KEY,
        message: "Invalid state key",
      });
    }

    try {
      const tempDir = await this.downloadAndExtractState(botId);
      if (!tempDir) {
        return Failure({
          code: BotStateErrorCode.KEY_NOT_FOUND,
          message: "State key not found",
        });
      }

      try {
        const filepath = path.join(tempDir, ".the0-state", `${key}.json`);

        // Check if file exists
        let stats;
        try {
          stats = await fs.stat(filepath);
        } catch {
          return Failure({
            code: BotStateErrorCode.KEY_NOT_FOUND,
            message: "State key not found",
          });
        }

        // Check file size before reading
        if (stats.size > this.maxStateFileSize) {
          this.logger.warn(
            { botId, key, size: stats.size, maxSize: this.maxStateFileSize },
            "State file exceeds maximum size",
          );
          return Failure({
            code: BotStateErrorCode.FILE_TOO_LARGE,
            message: `State file exceeds maximum size limit (${Math.round(this.maxStateFileSize / 1024 / 1024)}MB)`,
          });
        }

        const content = await fs.readFile(filepath, "utf-8");

        // Parse JSON with specific error handling
        let value: unknown;
        try {
          value = JSON.parse(content);
        } catch (parseError: any) {
          this.logger.error(
            { err: parseError, botId, key },
            "Invalid JSON in state file",
          );
          return Failure({
            code: BotStateErrorCode.INVALID_JSON,
            message: "State file contains invalid JSON",
          });
        }

        return Ok(value);
      } finally {
        // Cleanup temp directory
        await fs.rm(tempDir, { recursive: true, force: true });
      }
    } catch (error: any) {
      this.logger.error({ err: error, botId, key }, "Error getting state key");
      return Failure({
        code: BotStateErrorCode.STORAGE_ERROR,
        message: "Failed to get state key",
      });
    }
  }

  /**
   * Delete a specific state key.
   * Uses optimistic locking to prevent race conditions that could cause data loss.
   */
  async deleteKey(
    botId: string,
    key: string,
  ): Promise<Result<boolean, BotStateError>> {
    // Verify bot ownership
    const botResult = await this.botService.findOne(botId);
    if (!botResult.success) {
      return Failure({
        code: BotStateErrorCode.BOT_NOT_FOUND,
        message: "Bot not found or access denied",
      });
    }

    // Validate bot ID to prevent path traversal
    if (!this.isValidBotId(botId)) {
      return Failure({
        code: BotStateErrorCode.STORAGE_ERROR,
        message: "Invalid bot ID format",
      });
    }

    // Validate key using allowlist regex
    if (!this.isValidKey(key)) {
      return Failure({
        code: BotStateErrorCode.INVALID_KEY,
        message: "Invalid state key",
      });
    }

    try {
      // Download state with ETag for optimistic locking
      const downloadResult = await this.downloadAndExtractStateWithEtag(botId);
      if (!downloadResult) {
        return Ok(false); // No state exists
      }

      const { tempDir, etag } = downloadResult;

      try {
        const filepath = path.join(tempDir, ".the0-state", `${key}.json`);
        try {
          await fs.access(filepath);
        } catch {
          return Ok(false);
        }

        // Delete the file
        await fs.unlink(filepath);

        // Re-upload the modified state with optimistic locking
        const uploadSucceeded = await this.uploadStateWithLocking(
          botId,
          tempDir,
          etag,
        );

        if (!uploadSucceeded) {
          // Concurrent modification detected - return conflict error
          return Failure({
            code: BotStateErrorCode.CONCURRENT_MODIFICATION,
            message:
              "State was modified by another operation. Please retry the request.",
          });
        }

        return Ok(true);
      } finally {
        // Cleanup temp directory
        await fs.rm(tempDir, { recursive: true, force: true });
      }
    } catch (error: any) {
      this.logger.error({ err: error, botId, key }, "Error deleting state key");
      return Failure({
        code: BotStateErrorCode.STORAGE_ERROR,
        message: "Failed to delete state key",
      });
    }
  }

  /**
   * Clear all state for a bot.
   */
  async clearState(botId: string): Promise<Result<boolean, BotStateError>> {
    // Validate bot ID to prevent path traversal
    if (!this.isValidBotId(botId)) {
      return Failure({
        code: BotStateErrorCode.STORAGE_ERROR,
        message: "Invalid bot ID format",
      });
    }

    // Verify bot ownership
    const botResult = await this.botService.findOne(botId);
    if (!botResult.success) {
      return Failure({
        code: BotStateErrorCode.BOT_NOT_FOUND,
        message: "Bot not found or access denied",
      });
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
      return Failure({
        code: BotStateErrorCode.STORAGE_ERROR,
        message: "Failed to clear state",
      });
    }
  }

  /**
   * Download and extract bot state to a temporary directory.
   * Returns null if no state exists, otherwise returns tempDir and ETag for optimistic locking.
   */
  private async downloadAndExtractStateWithEtag(
    botId: string,
  ): Promise<StateDownloadResult | null> {
    const statePath = `${botId}/state.tar.gz`;

    // Check if state exists and get ETag for optimistic locking
    let stat: Minio.BucketItemStat;
    try {
      stat = await this.minioClient.statObject(this.stateBucket, statePath);
    } catch (error: any) {
      if (error.code === "NotFound") {
        return null;
      }
      throw error;
    }

    // Check archive size before download (DoS protection)
    if (stat.size > this.maxStateArchiveSize) {
      throw new Error(
        `State archive exceeds maximum size (${stat.size} bytes > ${this.maxStateArchiveSize} bytes)`,
      );
    }

    const etag = stat.etag;

    // Create temp directory
    const tempDir = await fs.mkdtemp(path.join(os.tmpdir(), "bot-state-"));
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
      await fs.unlink(tarPath);

      return { tempDir, etag };
    } catch (error) {
      // Cleanup on error
      await fs.rm(tempDir, { recursive: true, force: true });
      throw error;
    }
  }

  /**
   * Download and extract bot state to a temporary directory.
   * Returns null if no state exists.
   * @deprecated Use downloadAndExtractStateWithEtag for write operations to prevent race conditions
   */
  private async downloadAndExtractState(
    botId: string,
  ): Promise<string | null> {
    const result = await this.downloadAndExtractStateWithEtag(botId);
    return result?.tempDir ?? null;
  }

  /**
   * Upload state from a temporary directory back to MinIO with optimistic locking.
   * @param botId Bot ID
   * @param tempDir Temporary directory containing state
   * @param expectedEtag Optional ETag from download - if provided, upload will fail if state was modified
   * @returns true if upload succeeded, false if conflict detected (state was modified)
   */
  private async uploadStateWithLocking(
    botId: string,
    tempDir: string,
    expectedEtag?: string,
  ): Promise<boolean> {
    const statePath = `${botId}/state.tar.gz`;
    const tarPath = path.join(tempDir, "state.tar.gz");
    const stateDir = path.join(tempDir, ".the0-state");

    // Check if there's any state left
    try {
      await fs.access(stateDir);
    } catch {
      // No state directory, remove the object (with conflict check if needed)
      if (expectedEtag) {
        // Verify state hasn't changed before deleting
        try {
          const currentStat = await this.minioClient.statObject(
            this.stateBucket,
            statePath,
          );
          if (currentStat.etag !== expectedEtag) {
            this.logger.warn(
              { botId, expectedEtag, currentEtag: currentStat.etag },
              "Concurrent modification detected during state deletion",
            );
            return false; // Conflict - state was modified
          }
        } catch (error: any) {
          if (error.code !== "NotFound") {
            throw error;
          }
          // Object already deleted, that's fine
        }
      }
      try {
        await this.minioClient.removeObject(this.stateBucket, statePath);
      } catch {
        // Ignore errors when removing
      }
      return true;
    }

    const files = (await fs.readdir(stateDir)).filter((f) =>
      f.endsWith(".json"),
    );
    if (files.length === 0) {
      // No state files left, remove the object (with conflict check)
      if (expectedEtag) {
        try {
          const currentStat = await this.minioClient.statObject(
            this.stateBucket,
            statePath,
          );
          if (currentStat.etag !== expectedEtag) {
            this.logger.warn(
              { botId, expectedEtag, currentEtag: currentStat.etag },
              "Concurrent modification detected during state deletion",
            );
            return false;
          }
        } catch (error: any) {
          if (error.code !== "NotFound") {
            throw error;
          }
        }
      }
      try {
        await this.minioClient.removeObject(this.stateBucket, statePath);
      } catch {
        // Ignore errors when removing
      }
      return true;
    }

    // Verify state hasn't been modified since download (optimistic locking)
    if (expectedEtag) {
      try {
        const currentStat = await this.minioClient.statObject(
          this.stateBucket,
          statePath,
        );
        if (currentStat.etag !== expectedEtag) {
          this.logger.warn(
            { botId, expectedEtag, currentEtag: currentStat.etag },
            "Concurrent modification detected - aborting upload to prevent data loss",
          );
          return false; // Conflict - state was modified by another request
        }
      } catch (error: any) {
        if (error.code === "NotFound") {
          // Object was deleted - this is also a conflict since we expected it to exist
          this.logger.warn(
            { botId, expectedEtag },
            "State object was deleted during modification",
          );
          return false;
        }
        throw error;
      }
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
    return true;
  }

  /**
   * Upload state from a temporary directory back to MinIO.
   * @deprecated Use uploadStateWithLocking for write operations to prevent race conditions
   */
  private async uploadState(botId: string, tempDir: string): Promise<void> {
    await this.uploadStateWithLocking(botId, tempDir);
  }
}
