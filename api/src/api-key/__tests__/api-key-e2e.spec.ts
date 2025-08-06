import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication, ValidationPipe } from '@nestjs/common';
import request from 'supertest';

import { AuthGuard } from '@nestjs/passport';
import { Ok, Failure } from '@/common/result';
import { ApiKeyRepository } from '@/api-key/api-key.repository';
import { ApiKeyModule } from '@/api-key/api-key.module';
import { PassportModule } from '@nestjs/passport';
import { ConfigModule } from '@nestjs/config';

describe('ApiKeyController (e2e)', () => {
  let app: INestApplication;
  let mockGuard: any;
  let mockRepository: jest.Mocked<ApiKeyRepository>;

  const mockApiKey = {
    id: 'test-key-id',
    userId: 'test-user-123',
    name: 'Test API Key',
    keyValue: 'the0_1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef',
    key: 'the0_1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef',
    isActive: true,
    createdAt: new Date(),
    updatedAt: new Date(),
  };

  beforeAll(async () => {
    // Mock the JWT Auth Guard
    mockGuard = {
      canActivate: jest.fn(() => true),
    };

    // Mock repository methods
    const mockRepositoryMethods = {
      createApiKey: jest.fn(),
      findAll: jest.fn(),
      findOne: jest.fn(),
      deleteApiKey: jest.fn(),
      findByKey: jest.fn(),
      updateLastUsed: jest.fn(),
    };

    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [
        ConfigModule.forRoot({ isGlobal: true }),
        PassportModule.register({ defaultStrategy: 'jwt' }),
        ApiKeyModule,
      ],
    })
      .overrideGuard(AuthGuard())
      .useValue(mockGuard)
      .overrideProvider(ApiKeyRepository)
      .useValue(mockRepositoryMethods)
      .compile();
    app = moduleFixture.createNestApplication();
    app.useGlobalPipes(
      new ValidationPipe({
        whitelist: true,
        forbidNonWhitelisted: true,
        transform: true,
      }),
    );
    mockRepository = moduleFixture.get(ApiKeyRepository);
    await app.init();
  });

  beforeEach(() => {
    // Reset all mocks
    jest.clearAllMocks();

    // Mock request user for all tests
    jest.spyOn(mockGuard, 'canActivate').mockImplementation((context: any) => {
      const httpContext = context.switchToHttp();
      const request = httpContext.getRequest();
      request.user = { uid: 'test-user-123' }; // Updated to match your structure
      return true;
    });

    // Setup default mock responses
    mockRepository.findAll.mockResolvedValue(Ok([]));
    mockRepository.createApiKey.mockResolvedValue(Ok(mockApiKey));
  });

  it('/api-keys (POST) should create API key', async () => {
    return request(app.getHttpServer())
      .post('/api-keys')
      .send({ name: 'Test API Key' })
      .expect(201)
      .expect((res) => {
        expect(res.body.name).toBe('Test API Key');
        expect(res.body.key).toMatch(/^the0_[a-f0-9]{64}$/);
        expect(res.body.isActive).toBe(true);
        expect(mockRepository.createApiKey).toHaveBeenCalledWith(
          'test-user-123',
          'Test API Key',
        );
      });
  });

  it('/api-keys (POST) should fail with duplicate name', async () => {
    // Mock existing API key with same name
    mockRepository.findAll.mockResolvedValue(Ok([mockApiKey]));

    return request(app.getHttpServer())
      .post('/api-keys')
      .send({ name: 'Test API Key' })
      .expect(400)
      .expect((res) => {
        expect(res.body.message).toBe(
          'An API key with this name already exists',
        );
      });
  });

  it('/api-keys (POST) should validate input', async () => {
    return request(app.getHttpServer())
      .post('/api-keys')
      .send({ name: '' }) // Invalid empty name
      .expect(400);
  });

  it('/api-keys (GET) should return user API keys', async () => {
    mockRepository.findAll.mockResolvedValue(Ok([mockApiKey]));

    return request(app.getHttpServer())
      .get('/api-keys')
      .expect(200)
      .expect((res) => {
        expect(Array.isArray(res.body)).toBe(true);
        expect(res.body).toHaveLength(1);
        expect(res.body[0]).toHaveProperty('key');
        expect(res.body[0].key).toMatch(/^the0_[a-f0-9]{64}$/);
        expect(mockRepository.findAll).toHaveBeenCalledWith('test-user-123');
      });
  });

  it('/api-keys (GET) should handle empty result', async () => {
    mockRepository.findAll.mockResolvedValue(Ok([]));

    return request(app.getHttpServer())
      .get('/api-keys')
      .expect(200)
      .expect((res) => {
        expect(Array.isArray(res.body)).toBe(true);
        expect(res.body).toHaveLength(0);
      });
  });

  it('/api-keys/:id (GET) should return specific API key', async () => {
    mockRepository.findOne.mockResolvedValue(Ok(mockApiKey));

    return request(app.getHttpServer())
      .get('/api-keys/test-key-id')
      .expect(200)
      .expect((res) => {
        expect(res.body.id).toBe('test-key-id');
        expect(res.body).toHaveProperty('key');
        expect(res.body.key).toMatch(/^the0_[a-f0-9]{64}$/);
        expect(mockRepository.findOne).toHaveBeenCalledWith(
          'test-user-123',
          'test-key-id',
        );
      });
  });

  it('/api-keys/:id (GET) should return 404 for non-existent key', async () => {
    mockRepository.findOne.mockResolvedValue(Failure('Not found'));

    return request(app.getHttpServer())
      .get('/api-keys/non-existent-id')
      .expect(404)
      .expect((res) => {
        expect(res.body.message).toBe('Not found');
      });
  });

  it('/api-keys/:id (DELETE) should delete API key', async () => {
    mockRepository.deleteApiKey.mockResolvedValue(Ok(null));

    return request(app.getHttpServer())
      .delete('/api-keys/test-key-id')
      .expect(200)
      .expect((res) => {
        expect(res.body.message).toBe('API key deleted successfully');
        expect(mockRepository.deleteApiKey).toHaveBeenCalledWith(
          'test-user-123',
          'test-key-id',
        );
      });
  });

  it('/api-keys/:id (DELETE) should return 404 for non-existent key', async () => {
    mockRepository.deleteApiKey.mockResolvedValue(
      Failure('API key not found'),
    );

    return request(app.getHttpServer())
      .delete('/api-keys/non-existent-id')
      .expect(404)
      .expect((res) => {
        expect(res.body.message).toBe('API key not found');
      });
  });

  it('/api-keys/stats/summary (GET) should return statistics', async () => {
    const apiKeys = [mockApiKey, { ...mockApiKey, id: 'test-key-id-2' }];
    mockRepository.findAll.mockResolvedValue(Ok(apiKeys));

    return request(app.getHttpServer())
      .get('/api-keys/stats/summary')
      .expect(200)
      .expect((res) => {
        expect(res.body).toHaveProperty('total');
        expect(res.body).toHaveProperty('active');
        expect(typeof res.body.total).toBe('number');
        expect(typeof res.body.active).toBe('number');
        expect(res.body.total).toBe(2);
        expect(res.body.active).toBe(2);
      });
  });

  it('should handle repository errors gracefully', async () => {
    mockRepository.findAll.mockResolvedValue(
      Failure('Database connection error'),
    );

    return request(app.getHttpServer())
      .get('/api-keys')
      .expect(500)
      .expect((res) => {
        expect(res.body.message).toBe('Database connection error');
      });
  });

  afterAll(async () => {
    await app.close();
  });

  it('/api-keys (POST) should validate input', async () => {
    // Test empty name
    await request(app.getHttpServer())
      .post('/api-keys')
      .send({ name: '' })
      .expect(400)
      .expect((res) => {
        expect(res.body.message).toContain(
          'name must be longer than or equal to 3 characters',
        );
      });

    // Test missing name
    await request(app.getHttpServer())
      .post('/api-keys')
      .send({})
      .expect(400)
      .expect((res) => {
        expect(res.body.message).toContain('name should not be empty');
      });

    // Test name too long
    await request(app.getHttpServer())
      .post('/api-keys')
      .send({ name: 'a'.repeat(51) }) // 51 characters (max is 50)
      .expect(400)
      .expect((res) => {
        expect(res.body.message).toContain(
          'name must be shorter than or equal to 50 characters',
        );
      });

    // Test non-string name
    await request(app.getHttpServer())
      .post('/api-keys')
      .send({ name: 123 })
      .expect(400)
      .expect((res) => {
        expect(res.body.message).toContain('name must be a string');
      });
  });
});
