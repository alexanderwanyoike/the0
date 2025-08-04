import { Test, TestingModule } from '@nestjs/testing';
import { ExecutionContext } from '@nestjs/common';
import { AuthCombinedGuard } from '../auth-combined.guard';
import { AuthService } from '../auth.service';
import { ApiKeyService } from '../../api-key/api-key.service';
import { Ok, Failure } from '../../common/result';

describe('AuthCombinedGuard', () => {
  let guard: AuthCombinedGuard;
  let authService: AuthService;
  let apiKeyService: ApiKeyService;

  const mockAuthService = {
    validateToken: jest.fn(),
  };

  const mockApiKeyService = {
    validateApiKey: jest.fn(),
  };

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [
        AuthCombinedGuard,
        {
          provide: AuthService,
          useValue: mockAuthService,
        },
        {
          provide: ApiKeyService,
          useValue: mockApiKeyService,
        },
      ],
    }).compile();

    guard = module.get<AuthCombinedGuard>(AuthCombinedGuard);
    authService = module.get<AuthService>(AuthService);
    apiKeyService = module.get<ApiKeyService>(ApiKeyService);
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should be defined', () => {
    expect(guard).toBeDefined();
  });

  it('should allow access with valid JWT token', async () => {
    const mockContext = {
      switchToHttp: () => ({
        getRequest: () => ({
          headers: {
            authorization: 'Bearer valid-jwt-token',
          },
        }),
      }),
    } as ExecutionContext;

    const mockUser = {
      id: 'test-id',
      username: 'testuser',
      email: 'test@example.com',
      isActive: true,
      isEmailVerified: false,
    };

    mockAuthService.validateToken.mockResolvedValue(Ok(mockUser));

    const result = await guard.canActivate(mockContext);

    expect(result).toBe(true);
    expect(authService.validateToken).toHaveBeenCalledWith('valid-jwt-token');
  });

  it('should throw exception without JWT token', async () => {
    const mockContext = {
      switchToHttp: () => ({
        getRequest: () => ({
          headers: {
            'x-api-key': 'valid-api-key',
          },
        }),
      }),
    } as ExecutionContext;

    await expect(guard.canActivate(mockContext)).rejects.toThrow(
      'Authentication required. Provide Bearer JWT token.'
    );
  });

  it('should throw exception without any authentication', async () => {
    const mockContext = {
      switchToHttp: () => ({
        getRequest: () => ({
          headers: {},
        }),
      }),
    } as ExecutionContext;

    await expect(guard.canActivate(mockContext)).rejects.toThrow(
      'Authentication required. Provide Bearer JWT token.'
    );
  });
});