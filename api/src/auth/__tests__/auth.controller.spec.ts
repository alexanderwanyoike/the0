import { Test, TestingModule } from '@nestjs/testing';
import { UnauthorizedException } from '@nestjs/common';
import { AuthController } from '../auth.controller';
import { AuthService } from '../auth.service';
import { ApiKeyService } from '@/api-key/api-key.service';
import { Ok, Failure } from '../../common/result';

describe('AuthController', () => {
  let controller: AuthController;
  let authService: AuthService;

  const mockAuthService = {
    login: jest.fn(),
    register: jest.fn(),
    validateToken: jest.fn(),
  };

  const mockApiKeyService = {
    // Add any methods the AuthController might use from ApiKeyService
    createApiKey: jest.fn(),
    getUserApiKeys: jest.fn(),
    deleteApiKey: jest.fn(),
  };

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      controllers: [AuthController],
      providers: [
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

    controller = module.get<AuthController>(AuthController);
    authService = module.get<AuthService>(AuthService);
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should be defined', () => {
    expect(controller).toBeDefined();
  });

  describe('login', () => {
    it('should return success on valid credentials', async () => {
      const loginDto = {
        email: 'test@example.com',
        password: 'password123',
      };

      const mockResult = Ok({
        token: 'test-token',
        user: {
          id: 'test-id',
          username: 'testuser',
          email: 'test@example.com',
          isActive: true,
          isEmailVerified: false,
        },
      });

      mockAuthService.login.mockResolvedValue(mockResult);

      const result = await controller.login(loginDto);

      expect(result.success).toBe(true);
      expect(result.data.token).toBe('test-token');
      expect(authService.login).toHaveBeenCalledWith(loginDto);
    });

    it('should return error on invalid credentials', async () => {
      const loginDto = {
        email: 'test@example.com',
        password: 'wrongpassword',
      };

      const mockResult = Failure('Invalid credentials');
      mockAuthService.login.mockResolvedValue(mockResult);

      await expect(controller.login(loginDto)).rejects.toThrow(UnauthorizedException);
      await expect(controller.login(loginDto)).rejects.toThrow('Invalid credentials');
    });
  });

  describe('register', () => {
    it('should register user successfully', async () => {
      const registerDto = {
        username: 'newuser',
        email: 'new@example.com',
        password: 'password123',
      };

      const mockResult = Ok({
        token: 'test-token',
        user: {
          id: 'test-id',
          username: 'newuser',
          email: 'new@example.com',
          isActive: true,
          isEmailVerified: false,
        },
      });

      mockAuthService.register.mockResolvedValue(mockResult);

      const result = await controller.register(registerDto);

      expect(result.success).toBe(true);
      expect(result.data.token).toBe('test-token');
      expect(authService.register).toHaveBeenCalledWith(registerDto);
    });
  });

  describe('validate', () => {
    it('should validate token successfully', async () => {
      const validateDto = { token: 'valid-token' };

      const mockResult = Ok({
        id: 'test-id',
        username: 'testuser',
        email: 'test@example.com',
        isActive: true,
        isEmailVerified: false,
      });

      mockAuthService.validateToken.mockResolvedValue(mockResult);

      const result = await controller.validate(validateDto);

      expect(result.success).toBe(true);
      expect(authService.validateToken).toHaveBeenCalledWith('valid-token');
    });

    it('should return error for invalid token', async () => {
      const validateDto = { token: 'invalid-token' };

      const mockResult = Failure('Invalid token');
      mockAuthService.validateToken.mockResolvedValue(mockResult);

      await expect(controller.validate(validateDto)).rejects.toThrow(UnauthorizedException);
      await expect(controller.validate(validateDto)).rejects.toThrow('Invalid token');
    });
  });
});