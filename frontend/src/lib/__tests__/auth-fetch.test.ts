import { authFetch, setAuthFetchService } from '../auth-fetch';

describe('authFetch', () => {
  const mockAuthService = {
    getToken: jest.fn(),
    logout: jest.fn(),
  };
  const mockLogout = jest.fn();

  beforeEach(() => {
    jest.clearAllMocks();
    setAuthFetchService(mockAuthService, mockLogout);
  });

  describe('authentication errors', () => {
    it('should throw error when no auth service is set', async () => {
      setAuthFetchService(null as any, null as any);

      await expect(authFetch('/api/test')).rejects.toThrow('Auth service not initialized');
    });

    it('should throw error when no token is available', async () => {
      mockAuthService.getToken.mockReturnValue(null);

      await expect(authFetch('/api/test')).rejects.toThrow('Not authenticated');
    });

    it('should call centralized logout on 401 response', async () => {
      mockAuthService.getToken.mockReturnValue('valid-token');
      
      // Mock a 401 response by having the auth service return a token but simulating auth failure
      global.fetch = jest.fn().mockResolvedValue({
        status: 401,
        ok: false,
        clone: () => ({ status: 401, ok: false }),
      });

      await expect(authFetch('/api/test')).rejects.toThrow('Authentication failed');
      expect(mockLogout).toHaveBeenCalled();
    });

    it('should fallback to auth service logout when centralized logout not available', async () => {
      setAuthFetchService(mockAuthService, null as any);
      mockAuthService.getToken.mockReturnValue('valid-token');
      
      global.fetch = jest.fn().mockResolvedValue({
        status: 401,
        ok: false,
        clone: () => ({ status: 401, ok: false }),
      });

      await expect(authFetch('/api/test')).rejects.toThrow('Authentication failed');
      expect(mockAuthService.logout).toHaveBeenCalled();
    });

    it('should skip logout when skipTokenRefresh is true', async () => {
      mockAuthService.getToken.mockReturnValue('valid-token');
      
      global.fetch = jest.fn().mockResolvedValue({
        status: 401,
        ok: false,
        clone: () => ({ status: 401, ok: false }),
      });

      const response = await authFetch('/api/test', {
        skipTokenRefresh: true,
      });

      expect(mockLogout).not.toHaveBeenCalled();
      expect(response.status).toBe(401);
    });
  });

  describe('successful requests', () => {
    it('should return response for successful requests', async () => {
      mockAuthService.getToken.mockReturnValue('valid-token');
      
      global.fetch = jest.fn().mockResolvedValue({
        status: 200,
        ok: true,
        clone: () => ({ status: 200, ok: true }),
      });

      const response = await authFetch('/api/test');

      expect(response.status).toBe(200);
    });

    it('should return response for non-401 errors', async () => {
      mockAuthService.getToken.mockReturnValue('valid-token');
      
      global.fetch = jest.fn().mockResolvedValue({
        status: 500,
        ok: false,
        statusText: 'Internal Server Error',
        clone: () => ({ status: 500, ok: false }),
      });

      const response = await authFetch('/api/test');

      expect(response.status).toBe(500);
      expect(mockLogout).not.toHaveBeenCalled();
    });
  });

  describe('setAuthFetchService', () => {
    it('should register new auth service and logout function', async () => {
      const newAuthService = {
        getToken: jest.fn().mockReturnValue('new-token'),
        logout: jest.fn(),
      };
      const newLogout = jest.fn();

      setAuthFetchService(newAuthService, newLogout);
      
      global.fetch = jest.fn().mockResolvedValue({
        status: 200,
        ok: true,
        clone: () => ({ status: 200, ok: true }),
      });

      await authFetch('/api/test');

      expect(newAuthService.getToken).toHaveBeenCalled();
    });
  });
});