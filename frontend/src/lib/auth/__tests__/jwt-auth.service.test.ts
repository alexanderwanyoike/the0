import { JwtAuthService } from '../jwt-auth.service';
import { AuthUser } from '../types';

// Create a proper mock Response
const createMockResponse = (options: Partial<Response> = {}): Response => ({
  status: 200,
  ok: true,
  headers: new Headers(),
  statusText: 'OK',
  url: '',
  redirected: false,
  type: 'default',
  body: null,
  bodyUsed: false,
  clone: jest.fn(() => createMockResponse(options)),
  json: jest.fn(),
  text: jest.fn(),
  blob: jest.fn(),
  arrayBuffer: jest.fn(),
  formData: jest.fn(),
  ...options,
} as unknown as Response);

// Mock fetch globally
const mockFetch = jest.fn();
global.fetch = mockFetch;

describe('JwtAuthService', () => {
  let authService: JwtAuthService;
  const mockUser: AuthUser = {
    id: '1',
    username: 'testuser',
    email: 'test@example.com',
    firstName: 'Test',
    lastName: 'User',
    isActive: true,
    isEmailVerified: true,
  };

  beforeEach(() => {
    authService = new JwtAuthService();
    mockFetch.mockClear();
    localStorage.clear();
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  describe('login', () => {
    it('should login successfully with valid credentials', async () => {
      const mockResponse = {
        success: true,
        data: {
          token: 'mock-jwt-token',
          user: mockUser,
        },
        message: 'Login successful',
      };

      const mockResponseObj = createMockResponse({
        ok: true,
        json: jest.fn().mockResolvedValue(mockResponse),
      });
      mockFetch.mockResolvedValueOnce(mockResponseObj);

      const result = await authService.login({
        email: 'test@example.com',
        password: 'password123',
      });

      expect(result.success).toBe(true);
      expect(result.data?.token).toBe('mock-jwt-token');
      expect(result.data?.user).toEqual(mockUser);
      expect(localStorage.getItem('auth-token')).toBe('mock-jwt-token');
      expect(mockFetch).toHaveBeenCalledTimes(1);
    });

    it('should handle login failure', async () => {
      const mockResponseObj = createMockResponse({
        ok: false,
        status: 401,
        json: jest.fn().mockResolvedValue({ message: 'Invalid credentials' }),
      });
      mockFetch.mockResolvedValueOnce(mockResponseObj);

      const result = await authService.login({
        email: 'wrong@example.com',
        password: 'wrongpassword',
      });

      expect(result.success).toBe(false);
      expect(result.error).toBe('Invalid credentials');
      expect(localStorage.getItem('auth-token')).toBeNull();
    });

    it('should handle network errors', async () => {
      mockFetch.mockRejectedValueOnce(new Error('Network error'));

      const result = await authService.login({
        email: 'test@example.com',
        password: 'password123',
      });

      expect(result.success).toBe(false);
      expect(result.error).toBe('Login failed. Please try again.');
    });
  });

  describe('register', () => {
    it('should register successfully with valid data', async () => {
      const mockResponse = {
        success: true,
        data: {
          token: 'mock-jwt-token',
          user: mockUser,
        },
        message: 'Registration successful',
      };

      const mockResponseObj = createMockResponse({
        ok: true,
        json: jest.fn().mockResolvedValue(mockResponse),
      });
      mockFetch.mockResolvedValueOnce(mockResponseObj);

      const result = await authService.register({
        username: 'testuser',
        email: 'test@example.com',
        password: 'password123',
        firstName: 'Test',
        lastName: 'User',
      });

      expect(result.success).toBe(true);
      expect(result.data?.token).toBe('mock-jwt-token');
      expect(result.data?.user).toEqual(mockUser);
      expect(localStorage.getItem('auth-token')).toBe('mock-jwt-token');
    });

    it('should handle registration failure', async () => {
      const mockResponseObj = createMockResponse({
        ok: false,
        status: 400,
        json: jest.fn().mockResolvedValue({ message: 'User already exists' }),
      });
      mockFetch.mockResolvedValueOnce(mockResponseObj);

      const result = await authService.register({
        username: 'existinguser',
        email: 'existing@example.com',
        password: 'password123',
      });

      expect(result.success).toBe(false);
      expect(result.error).toBe('User already exists');
    });
  });

  describe('validateToken', () => {
    it('should validate token successfully', async () => {
      const mockResponse = {
        success: true,
        data: mockUser,
        message: 'Token is valid',
      };

      const mockResponseObj = createMockResponse({
        ok: true,
        json: jest.fn().mockResolvedValue(mockResponse),
      });
      mockFetch.mockResolvedValueOnce(mockResponseObj);

      const result = await authService.validateToken('valid-token');

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockUser);
      expect(mockFetch).toHaveBeenCalledTimes(1);
    });

    it('should handle invalid token', async () => {
      const mockResponseObj = createMockResponse({
        ok: false,
        status: 401,
        json: jest.fn().mockResolvedValue({ message: 'Invalid token' }),
      });
      mockFetch.mockResolvedValueOnce(mockResponseObj);

      const result = await authService.validateToken('invalid-token');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Invalid token');
    });
  });

  describe('getCurrentUser', () => {
    it('should get current user successfully', async () => {
      localStorage.setItem('auth-token', 'valid-token');
      
      const mockResponse = {
        success: true,
        data: mockUser,
        message: 'User retrieved successfully',
      };

      const mockResponseObj = createMockResponse({
        ok: true,
        json: jest.fn().mockResolvedValue(mockResponse),
      });
      mockFetch.mockResolvedValueOnce(mockResponseObj);

      const result = await authService.getCurrentUser();

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockUser);
      expect(mockFetch).toHaveBeenCalledTimes(1);
    });

    it('should return error when no token exists', async () => {
      const result = await authService.getCurrentUser();

      expect(result.success).toBe(false);
      expect(result.error).toBe('No authentication token found');
      expect(mockFetch).not.toHaveBeenCalled();
    });
  });

  describe('logout', () => {
    it('should logout successfully', () => {
      localStorage.setItem('auth-token', 'some-token');
      
      authService.logout();
      
      expect(localStorage.getItem('auth-token')).toBeNull();
    });
  });

  describe('getToken', () => {
    it('should return stored token', () => {
      localStorage.setItem('auth-token', 'stored-token');
      
      const token = authService.getToken();
      
      expect(token).toBe('stored-token');
    });

    it('should return null when no token exists', () => {
      const token = authService.getToken();
      
      expect(token).toBeNull();
    });
  });

  describe('isAuthenticated', () => {
    it('should return true when token exists', () => {
      localStorage.setItem('auth-token', 'some-token');
      
      expect(authService.isAuthenticated()).toBe(true);
    });

    it('should return false when no token exists', () => {
      expect(authService.isAuthenticated()).toBe(false);
    });
  });
});