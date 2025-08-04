import { Test, TestingModule } from '@nestjs/testing';
import { BacktestController } from '../backtest.controller';
import { BacktestService } from '../backtest.service';
import { BacktestRepository } from '../backtest.repository';
import { AuthCombinedGuard } from '@/auth/auth-combined.guard';
import { Backtest } from '../entities/backtest.entity';
import { REQUEST } from '@nestjs/core';
import { BacktestValidator } from '../backtest.validator';
import { CustomBotService } from '@/custom-bot/custom-bot.service';
import { Ok } from '@/common';

describe('BacktestController', () => {
  let controller: BacktestController;
  let repository: BacktestRepository;
  let module: TestingModule;
  const uid = 'test-user-id';

  const mockCustomBot = {
    id: 'custom-bot-id',
    name: 'test-bot',
    version: '1.0.0',
    userId: 'test-user-id', // Make the test user the owner
    status: 'approved',
    marketplace: { isPublished: true },
    config: {
      schema: {
        backtest: {
          type: 'object',
          properties: {
            type: { type: 'string' },
            version: { type: 'string' },
          },
        },
      },
    },
  };

  beforeEach(async () => {
    const mockForceGuard = { canActivate: jest.fn(() => true) };
    module = await Test.createTestingModule({
      controllers: [BacktestController],
      providers: [
        {
          provide: BacktestService,
          useValue: {
            create: jest.fn(),
            findAll: jest.fn(),
            findOne: jest.fn(),
            remove: jest.fn(),
          },
        },
        {
          provide: BacktestRepository,
          useValue: {
            create: jest.fn(),
            findAll: jest.fn(),
            findOne: jest.fn(),
            remove: jest.fn(),
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
      ],
    })
      .overrideGuard(AuthCombinedGuard)
      .useValue(mockForceGuard)
      .overrideProvider(REQUEST)
      .useValue({ user: { uid } })
      .compile();

    controller = module.get<BacktestController>(BacktestController);
    repository = module.get<BacktestRepository>(BacktestRepository);
  });

  it('should be able to create a backtest', async () => {
    const backtest = {
      name: 'Test backtest',
      config: {
        type: 'scheduled/test-bot',
        version: '1.0.0',
      },
    } as Backtest;

    const service = module.get<BacktestService>(BacktestService);
    (service.create as jest.Mock).mockResolvedValue({
      success: true,
      error: null,
      data: {
        id: 'test-id',
        ...backtest,
        customBotId: mockCustomBot.id,
      },
    });

    const result = await controller.create(backtest);
    expect(result).toEqual({
      id: 'test-id',
      ...backtest,
      customBotId: mockCustomBot.id,
    });
    expect(service.create).toHaveBeenCalledWith(backtest);
  });

  it('should be able to get all backtests', async () => {
    const backtests = [
      {
        id: 'test-id',
        name: 'Test backtest',
        config: {
          type: 'scheduled/test-bot',
          version: '1.0.0',
        },
      },
    ] as Backtest[];

    const service = module.get<BacktestService>(BacktestService);
    (service.findAll as jest.Mock).mockResolvedValue({
      success: true,
      error: null,
      data: backtests,
    });

    const result = await controller.findAll();
    expect(result).toEqual(backtests);
  });

  it('should be able to get a backtest by id', async () => {
    const backtest = {
      id: 'test-id',
      name: 'Test backtest',
      config: {
        type: 'scheduled/test-bot',
        version: '1.0.0',
      },
    } as Backtest;

    const service = module.get<BacktestService>(BacktestService);
    (service.findOne as jest.Mock).mockResolvedValue({
      success: true,
      error: null,
      data: backtest,
    });

    const result = await controller.findOne('test-id');
    expect(result).toEqual(backtest);
  });

  it('should be able to remove a backtest', async () => {
    const service = module.get<BacktestService>(BacktestService);
    (service.remove as jest.Mock).mockResolvedValue({
      success: true,
      error: null,
      data: null,
    });

    const result = await controller.remove('test-id');
    expect(result).toBeUndefined();
    expect(service.remove).toHaveBeenCalledWith('test-id');
  });
});
