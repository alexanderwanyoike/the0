import { Inject, Injectable, Scope } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import * as Minio from 'minio';
import { CreateBacktestDto } from './dto/create-backtest.dto';
import { BacktestRepository } from './backtest.repository';
import { REQUEST } from '@nestjs/core';
import { Result, Failure, Ok } from '../common';
import { Backtest } from './entities/backtest.entity';
import { BacktestValidator } from './backtest.validator';
import { CustomBotService } from '@/custom-bot/custom-bot.service';
import { CustomBot } from '@/custom-bot/custom-bot.types';
import { NatsService } from '@/nats/nats.service';
import { BOT_TYPE_PATTERN } from '@/bot/bot.constants';
import { BACKTEST_TOPICS } from './backtest.constants';
// FeatureGateService removed for OSS version

@Injectable({ scope: Scope.REQUEST })
export class BacktestService {
  private minioClient: Minio.Client;
  private backtestBucket: string;

  constructor(
    @Inject(REQUEST) private readonly request: any,
    private readonly configService: ConfigService,
    private readonly backtestRepository: BacktestRepository,
    private readonly backtestValidator: BacktestValidator,
    private readonly customBotService: CustomBotService,
    private readonly natsService: NatsService,
  ) {
    this.minioClient = new Minio.Client({
      endPoint: this.configService.get<string>('MINIO_ENDPOINT') || 'localhost',
      port: parseInt(this.configService.get<string>('MINIO_PORT') || '9000'),
      useSSL: this.configService.get<string>('MINIO_USE_SSL') === 'true',
      accessKey: this.configService.get<string>('MINIO_ACCESS_KEY') || 'minioadmin',
      secretKey: this.configService.get<string>('MINIO_SECRET_KEY') || 'minioadmin',
    });
    this.backtestBucket = this.configService.get<string>('BACKTEST_BUCKET') || 'backtests';
  }

  async create(
    createBacktestDto: CreateBacktestDto,
  ): Promise<Result<Backtest, string>> {
    const { uid } = this.request.user;

    // Feature gates removed for OSS version

    // Validate configuration and get custom bot
    const validationResult = await this.validateBacktestTypeAndConfig(
      createBacktestDto.config,
    );
    if (!validationResult.success) {
      return Failure<Backtest, string>(validationResult.error);
    }

    const customBot = validationResult.data;

    // Create backtest with customBotId
    const result = await this.backtestRepository.create({
      ...createBacktestDto,
      status: 'pending',
      userId: uid,
      customBotId: customBot.id,
    });
    
    if (result.success) {
      // Create backtest payload for the0-runtime
      const backtestPayload = {
        ID: result.data.id,
        Config: {
          ...createBacktestDto.config,
          customBotId: customBot.id,
        },
        Custom: {
          Config: customBot.config,
          CreatedAt: customBot.createdAt,
          UpdatedAt: customBot.updatedAt,
          FilePath: customBot.filePath || '',
          Version: customBot.version,
        },
      };
      
      // Publish backtest creation event to the0-runtime
      await this.natsService.publish(BACKTEST_TOPICS.CREATED, backtestPayload);
    }
    
    return result;
  }

  findAll(): Promise<Result<Backtest[], string>> {
    const { uid } = this.request.user;
    return this.backtestRepository.findAll(uid);
  }

  async findOne(id: string): Promise<Result<Backtest, string>> {
    const { uid } = this.request.user;
    const result = await this.backtestRepository.findOne(uid, id);

    if (!result.success) {
      return result;
    }

    const backtest = result.data;

    // Load analysis for completed and failed backtests (failed backtests may contain error info)
    console.log(`🔍 Backtest ${id} status: ${backtest.status}`);
    if (backtest.status === 'completed' || backtest.status === 'failed') {
      console.log(`📥 Loading analysis for ${backtest.status} backtest: ${id}`);
      const analysisResult = await this.loadAnalysisFromGCS(id);
      console.log(`📊 Analysis load result for ${id}: success=${analysisResult.success}, data=${analysisResult.success ? 'present' : 'none'}`);
      if (analysisResult.success) {
        backtest.analysis = analysisResult.data;
        console.log(`✅ Analysis attached to backtest ${id}`);
      } else {
        console.log(`❌ Analysis load failed for ${id}: ${analysisResult.error}`);
      }
      // Continue even if analysis loading fails - just omit the field
    } else {
      console.log(`⏸️ Skipping analysis load for ${id} (status: ${backtest.status})`);
    }

    return Ok(backtest);
  }

  remove(id: string): Promise<Result<void, string>> {
    const { uid } = this.request.user;
    return this.backtestRepository.remove(uid, id);
  }

  async getBacktestLogs(backtestId: string): Promise<Result<string, string>> {
    // Validate user has access to this backtest first
    const { uid } = this.request.user;
    const backtestResult = await this.backtestRepository.findOne(
      uid,
      backtestId,
    );
    if (!backtestResult.success) {
      return Failure('Backtest not found or access denied');
    }

    try {
      const objectName = `${backtestId}/logs.txt`;
      const stream = await this.minioClient.getObject(this.backtestBucket, objectName);
      
      let content = '';
      stream.on('data', (chunk) => {
        content += chunk.toString();
      });
      
      return new Promise((resolve) => {
        stream.on('end', () => {
          resolve(Ok(content));
        });
        stream.on('error', (error) => {
          resolve(Failure(`Failed to load logs: ${error.message}`));
        });
      });
    } catch (error: any) {
      return Failure(`Failed to load logs: ${error.message}`);
    }
  }

  private async loadAnalysisFromGCS(
    backtestId: string,
  ): Promise<Result<any, string>> {
    console.log(`🔍 Loading analysis for backtest: ${backtestId}`);
    try {
      const objectName = `${backtestId}/analysis.json`;
      console.log(`📁 Fetching object: ${objectName} from bucket: ${this.backtestBucket}`);
      const stream = await this.minioClient.getObject(this.backtestBucket, objectName);
      
      let content = '';
      stream.on('data', (chunk) => {
        content += chunk.toString();
      });
      
      return new Promise((resolve) => {
        stream.on('end', () => {
          console.log(`✅ Successfully loaded analysis for ${backtestId}, content length: ${content.length}`);
          try {
            const analysisData = JSON.parse(content);
            console.log(`📊 Analysis parsed successfully, has keys: ${Object.keys(analysisData)}`);
            resolve(Ok(analysisData));
          } catch (parseError) {
            console.log(`❌ Failed to parse analysis JSON for ${backtestId}: ${parseError.message}`);
            resolve(Failure(`Failed to parse analysis JSON: ${parseError.message}`));
          }
        });
        stream.on('error', (error) => {
          console.log(`❌ Stream error loading analysis for ${backtestId}: ${error.message}, code: ${(error as any).code}`);
          if ((error as any).code === 'NoSuchKey') {
            resolve(Ok(null)); // Return null if file doesn't exist
          } else {
            resolve(Failure(`Failed to load analysis: ${error.message}`));
          }
        });
      });
    } catch (error: any) {
      console.log(`❌ Exception loading analysis for ${backtestId}: ${error.message}, code: ${error.code}`);
      if (error.code === 'NoSuchKey') {
        return Ok(null); // Return null if file doesn't exist
      }
      return Failure(`Failed to load analysis: ${error.message}`);
    }
  }

  private async validateBacktestTypeAndConfig(
    config: any,
  ): Promise<Result<CustomBot, string>> {
    const { uid } = this.request.user;
    const { type, version } = config;

    if (!type) {
      return Failure<CustomBot, string>('Bot type is required');
    }

    if (!version) {
      return Failure<CustomBot, string>('Bot version is required');
    }

    // Check that type is in the correct format
    if (!BOT_TYPE_PATTERN.test(type)) {
      return Failure<CustomBot, string>(
        'Invalid bot type format. Expected format: type/name',
      );
    }

    // Extract vendor, type, and name from the bot type
    const [, name] = type.split('/');

    // Get custom bot
    const customBotResult =
      await this.customBotService.getGlobalSpecificVersion(name, version);
    if (!customBotResult.success) {
      return Failure<CustomBot, string>(
        `Custom bot not found: ${name}@${version}`,
      );
    }

    const customBot = customBotResult.data;

    // Check if bot has backtest entrypoint and schema
    if (!customBot.config.entrypoints.backtest) {
      return Failure<CustomBot, string>(
        'Bot does not support backtesting: missing backtest entrypoint',
      );
    }

    if (!customBot.config.schema.backtest) {
      return Failure<CustomBot, string>(
        'Bot does not support backtesting: missing backtest schema',
      );
    }

    // Check authorization (different from bot module)
    const authResult = await this.checkBacktestAuthorization(uid, customBot);
    if (!authResult.success) {
      return Failure<CustomBot, string>(authResult.error);
    }

    // Validate with custom bot's backtest schema
    const validationResult = await this.backtestValidator.validate(
      config,
      customBot,
    );
    if (!validationResult.success) {
      return Failure<CustomBot, string>(
        `Validation failed: ${validationResult.error.join(', ')}`,
      );
    }

    return Ok(customBot);
  }

  private async checkBacktestAuthorization(
    userId: string,
    customBot: CustomBot,
  ): Promise<Result<boolean, string>> {
    // Rule 1: Owner can backtest approved bots
    if (customBot.userId === userId && customBot.status === 'approved') {
      return Ok(true);
    }

    // Rule 2: Anyone can backtest published bots
    if (
      customBot.status === 'published' &&
      customBot.marketplace?.isPublished
    ) {
      return Ok(true);
    }

    // Rule 3: Reject all other cases
    return Failure<boolean, string>(
      'Insufficient permissions: Can only backtest published bots or your own approved bots',
    );
  }
}
