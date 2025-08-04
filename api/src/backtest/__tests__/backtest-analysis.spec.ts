import { Test, TestingModule } from '@nestjs/testing';
import { ConfigService } from '@nestjs/config';
import { BacktestService } from '../backtest.service';
import { BacktestRepository } from '../backtest.repository';
import { BacktestValidator } from '../backtest.validator';
import { CustomBotService } from '@/custom-bot/custom-bot.service';
import { NatsService } from '@/nats/nats.service';
import { REQUEST } from '@nestjs/core';
import { Ok, Failure } from '@/common';
import { EventEmitter } from 'events';

describe('BacktestService Analysis Loading', () => {
  let service: BacktestService;
  let configService: jest.Mocked<ConfigService>;
  let mockRepository: jest.Mocked<BacktestRepository>;

  const mockMinioClient = {
    getObject: jest.fn(),
  };

  const uid = 'test-user-id';

  const createMockStream = (content: string) => {
    const stream = new EventEmitter();
    setTimeout(() => {
      stream.emit('data', Buffer.from(content));
      stream.emit('end');
    }, 0);
    return stream;
  };

  const createErrorStream = (error: Error) => {
    const stream = new EventEmitter();
    setTimeout(() => {
      stream.emit('error', error);
    }, 0);
    return stream;
  };

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [
        BacktestService,
        {
          provide: ConfigService,
          useValue: {
            get: jest.fn().mockReturnValue('test-bucket'),
          },
        },
        {
          provide: BacktestRepository,
          useValue: {
            findOne: jest.fn(),
          },
        },
        {
          provide: BacktestValidator,
          useValue: {
            validate: jest.fn(),
          },
        },
        {
          provide: CustomBotService,
          useValue: {
            getGlobalSpecificVersion: jest.fn(),
          },
        },
        {
          provide: NatsService,
          useValue: {
            publish: jest.fn().mockResolvedValue(Ok(null)),
          },
        },
      ],
    })
      .overrideProvider(REQUEST)
      .useValue({ user: { uid } })
      .compile();

    service = await module.resolve<BacktestService>(BacktestService);
    configService = module.get(ConfigService);
    mockRepository = module.get(BacktestRepository);

    // Mock the MinIO client
    (service as any).minioClient = mockMinioClient;
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  describe('loadAnalysisFromGCS', () => {
    it('should load analysis successfully for variation 1 format', async () => {
      const mockAnalysis = {
        status: 'success',
        results: {
          metrics: { total_return: 0.23, sharpe_ratio: 1.45 },
          plots: [{ data: [], layout: {} }],
          tables: [],
        },
      };

      mockMinioClient.getObject.mockResolvedValue(createMockStream(JSON.stringify(mockAnalysis)));

      const result = await (service as any).loadAnalysisFromGCS('test-id');

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockAnalysis);
      expect(mockMinioClient.getObject).toHaveBeenCalledWith(
        'test-bucket',
        'backtests/test-id/analysis.json',
      );
    });

    it('should load analysis successfully for variation 2 format', async () => {
      const mockAnalysis = {
        metrics: { total_return: 0.23, sharpe_ratio: 1.45 },
        plots: [{ data: [], layout: {} }],
        tables: [],
        status: 'success',
      };

      mockMinioClient.getObject.mockResolvedValue(createMockStream(JSON.stringify(mockAnalysis)));

      const result = await (service as any).loadAnalysisFromGCS('test-id');

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockAnalysis);
    });

    it('should handle missing analysis file gracefully', async () => {
      const noSuchKeyError = new Error('The specified key does not exist.');
      (noSuchKeyError as any).code = 'NoSuchKey';
      
      mockMinioClient.getObject.mockResolvedValue(createErrorStream(noSuchKeyError));

      const result = await (service as any).loadAnalysisFromGCS('test-id');

      expect(result.success).toBe(true);
      expect(result.data).toBeNull();
    });

    it('should handle JSON parsing errors', async () => {
      mockMinioClient.getObject.mockResolvedValue(createMockStream('invalid json'));

      const result = await (service as any).loadAnalysisFromGCS('test-id');

      expect(result.success).toBe(false);
      expect(result.error).toContain('Failed to parse analysis JSON');
    });

    it('should handle MinIO download errors', async () => {
      const networkError = new Error('Network error');
      mockMinioClient.getObject.mockResolvedValue(createErrorStream(networkError));

      const result = await (service as any).loadAnalysisFromGCS('test-id');

      expect(result.success).toBe(false);
      expect(result.error).toContain('Failed to load analysis: Network error');
    });

    it('should handle analysis with plots as JSON strings', async () => {
      const mockAnalysis = {
        metrics: { total_return: 0.23 },
        plots: ['{"data": [], "layout": {}}'],
        tables: [],
        status: 'success',
      };

      mockMinioClient.getObject.mockResolvedValue(createMockStream(JSON.stringify(mockAnalysis)));

      const result = await (service as any).loadAnalysisFromGCS('test-id');

      expect(result.success).toBe(true);
      expect(result.data.plots[0]).toBe('{"data": [], "layout": {}}');
    });
  });

  describe('findOne with analysis loading', () => {
    const createMockBacktest = (status = 'completed') => ({
      id: 'test-id',
      name: 'Test Backtest',
      status,
      userId: uid,
      config: {},
      analysis: null,
      createdAt: new Date(),
      updatedAt: new Date(),
      customBotId: 'bot-id',
    });

    it('should include analysis for completed backtests', async () => {
      const mockAnalysisData = {
        metrics: { total_return: 0.23 },
        plots: [],
        tables: [],
      };

      mockRepository.findOne.mockResolvedValue(
        Ok(createMockBacktest('completed')),
      );
      mockMinioClient.getObject.mockResolvedValue(createMockStream(JSON.stringify(mockAnalysisData)));

      const result = await service.findOne('test-id');

      expect(result.success).toBe(true);
      expect(result.data.analysis).toEqual(mockAnalysisData);
    });

    it('should not load analysis for non-completed backtests', async () => {
      const runningBacktest = createMockBacktest('running');
      mockRepository.findOne.mockResolvedValue(Ok(runningBacktest));

      const result = await service.findOne('test-id');

      expect(result.success).toBe(true);
      expect(result.data.analysis).toBeNull();
      expect(mockMinioClient.getObject).not.toHaveBeenCalled();
    });

    it('should continue even if analysis loading fails', async () => {
      const completedBacktest = createMockBacktest('completed');
      mockRepository.findOne.mockResolvedValue(Ok(completedBacktest));
      
      const networkError = new Error('Network error');
      mockMinioClient.getObject.mockResolvedValue(createErrorStream(networkError));

      const result = await service.findOne('test-id');

      expect(result.success).toBe(true);
      expect(result.data.analysis).toBeNull();
    });

    it('should handle repository errors', async () => {
      mockRepository.findOne.mockResolvedValue(Failure('Database error'));

      const result = await service.findOne('test-id');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Database error');
    });

    it('should handle missing analysis file gracefully for completed backtest', async () => {
      const completedBacktest = createMockBacktest('completed');
      mockRepository.findOne.mockResolvedValue(Ok(completedBacktest));
      
      const noSuchKeyError = new Error('The specified key does not exist.');
      (noSuchKeyError as any).code = 'NoSuchKey';
      mockMinioClient.getObject.mockResolvedValue(createErrorStream(noSuchKeyError));

      const result = await service.findOne('test-id');

      expect(result.success).toBe(true);
      expect(result.data.analysis).toBeNull();
    });

    it('should set analysis to null when file exists but loading fails', async () => {
      const completedBacktest = createMockBacktest('completed');
      mockRepository.findOne.mockResolvedValue(Ok(completedBacktest));
      mockMinioClient.getObject.mockResolvedValue(createMockStream('invalid json'));

      const result = await service.findOne('test-id');

      expect(result.success).toBe(true);
      expect(result.data.analysis).toBeNull();
    });
  });
});