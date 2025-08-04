import { Test, TestingModule } from '@nestjs/testing';
import { ApiKeyService } from '@/api-key/api-key.service';
import { ApiKeyRepository } from '@/api-key/api-key.repository';
import { CreateApiKeyDto } from '../dto/create-api-key.dto';
import { Ok, Failure } from '@/common/result';

describe('ApiKeyService', () => {
  let service: ApiKeyService;
  let repository: jest.Mocked<ApiKeyRepository>;

  const mockApiKey = {
    id: 'test-id',
    userId: 'user-123',
    name: 'Test API Key',
    key: 'the0_abcdef123456789abcdef123456789abcdef123456789abcdef123456789',
    isActive: true,
    createdAt: new Date(),
    updatedAt: new Date(),
  };

  beforeEach(async () => {
    const mockRepository = {
      createApiKey: jest.fn(),
      findAll: jest.fn(),
      findOne: jest.fn(),
      deleteApiKey: jest.fn(),
      findByKey: jest.fn(),
      updateLastUsed: jest.fn(),
    };

    const module: TestingModule = await Test.createTestingModule({
      providers: [
        ApiKeyService,
        {
          provide: ApiKeyRepository,
          useValue: mockRepository,
        },
      ],
    }).compile();

    service = module.get<ApiKeyService>(ApiKeyService);
    repository = module.get(ApiKeyRepository);
  });

  describe('createApiKey', () => {
    it('should create an API key successfully', async () => {
      const createDto: CreateApiKeyDto = { name: 'Test API Key' };
      repository.findAll.mockResolvedValue(Ok([]));
      repository.createApiKey.mockResolvedValue(Ok(mockApiKey));

      const result = await service.createApiKey('user-123', createDto);

      expect(result.success).toBe(true);
      expect(result.data.name).toBe('Test API Key');
      expect(result.data.key).toBe('the0_abcdef123456789abcdef123456789abcdef123456789abcdef123456789');
      expect(repository.createApiKey).toHaveBeenCalledWith(
        'user-123',
        'Test API Key',
      );
    });

    it('should fail if API key name already exists', async () => {
      const createDto: CreateApiKeyDto = { name: 'Test API Key' };
      repository.findAll.mockResolvedValue(Ok([mockApiKey]));

      const result = await service.createApiKey('user-123', createDto);

      expect(result.success).toBe(false);
      expect(result.error).toBe('An API key with this name already exists');
    });

    it('should handle repository errors', async () => {
      const createDto: CreateApiKeyDto = { name: 'Test API Key' };
      repository.findAll.mockResolvedValue(Failure('Database error'));

      const result = await service.createApiKey('user-123', createDto);

      expect(result.success).toBe(false);
      expect(result.error).toBe('Database error');
    });
  });

  describe('getUserApiKeys', () => {
    it('should return user API keys with full key', async () => {
      repository.findAll.mockResolvedValue(Ok([mockApiKey]));

      const result = await service.getUserApiKeys('user-123');

      expect(result.success).toBe(true);
      expect(result.data).toHaveLength(1);
      expect(result.data[0].key).toBe('the0_abcdef123456789abcdef123456789abcdef123456789abcdef123456789');
    });

    it('should handle repository errors', async () => {
      repository.findAll.mockResolvedValue(Failure('Database error'));

      const result = await service.getUserApiKeys('user-123');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Database error');
    });
  });

  describe('getApiKeyById', () => {
    it('should return specific API key with full key', async () => {
      repository.findOne.mockResolvedValue(Ok(mockApiKey));

      const result = await service.getApiKeyById('user-123', 'test-id');

      expect(result.success).toBe(true);
      expect(result.data.key).toBe('the0_abcdef123456789abcdef123456789abcdef123456789abcdef123456789');
    });

    it('should handle not found', async () => {
      repository.findOne.mockResolvedValue(Failure('Not found'));

      const result = await service.getApiKeyById('user-123', 'test-id');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Not found');
    });
  });

  describe('deleteApiKey', () => {
    it('should delete API key successfully', async () => {
      repository.deleteApiKey.mockResolvedValue(Ok(null));

      const result = await service.deleteApiKey('user-123', 'test-id');

      expect(result.success).toBe(true);
      expect(repository.deleteApiKey).toHaveBeenCalledWith(
        'user-123',
        'test-id',
      );
    });

    it('should handle repository errors', async () => {
      repository.deleteApiKey.mockResolvedValue(
        Failure('API key not found'),
      );

      const result = await service.deleteApiKey('user-123', 'test-id');

      expect(result.success).toBe(false);
      expect(result.error).toBe('API key not found');
    });
  });

  describe('validateApiKey', () => {
    it('should validate API key and update last used', async () => {
      repository.findByKey.mockResolvedValue(Ok(mockApiKey));
      repository.updateLastUsed.mockResolvedValue(Ok(null));

      const result = await service.validateApiKey('the0_abcdef123456789');

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockApiKey);
      expect(repository.updateLastUsed).toHaveBeenCalledWith('test-id');
    });

    it('should handle invalid API key', async () => {
      repository.findByKey.mockResolvedValue(
        Failure('API key not found or inactive'),
      );

      const result = await service.validateApiKey('invalid-key');

      expect(result.success).toBe(false);
      expect(result.error).toBe('API key not found or inactive');
    });
  });

  describe('getApiKeyStats', () => {
    it('should return API key statistics', async () => {
      const apiKeys = [mockApiKey, { ...mockApiKey, id: 'test-id-2' }];
      repository.findAll.mockResolvedValue(Ok(apiKeys));

      const result = await service.getApiKeyStats('user-123');

      expect(result.success).toBe(true);
      expect(result.data.total).toBe(2);
      expect(result.data.active).toBe(2);
    });
  });
});
