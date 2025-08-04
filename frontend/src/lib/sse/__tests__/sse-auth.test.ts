import {
  createAuthenticatedSSEUrl,
  validateSSEAuth,
  handleSSEAuthError,
  getSSEAuthHeaders,
  checkSSEPermission,
  setSSEAuthService,
} from '../sse-auth';

describe('SSE Auth', () => {
  const mockAuthService = {
    getToken: jest.fn(),
    logout: jest.fn(),
  };
  const mockLogout = jest.fn();

  beforeEach(() => {
    jest.clearAllMocks();
    setSSEAuthService(mockAuthService, mockLogout);
    
    // Mock console methods
    jest.spyOn(console, 'debug').mockImplementation(() => {});
    jest.spyOn(console, 'error').mockImplementation(() => {});
    jest.spyOn(console, 'warn').mockImplementation(() => {});
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  describe('createAuthenticatedSSEUrl', () => {
    it('should create authenticated SSE URL when token exists', () => {
      mockAuthService.getToken.mockReturnValue('valid-token');

      const result = createAuthenticatedSSEUrl('/api/stream');

      expect(result).toEqual({
        success: true,
        data: {
          url: '/api/stream',
          isAuthenticated: true,
        },
      });
    });

    it('should create authenticated SSE URL with base URL', () => {
      mockAuthService.getToken.mockReturnValue('valid-token');

      const result = createAuthenticatedSSEUrl('/stream', { baseUrl: 'http://localhost:3000/api' });

      expect(result).toEqual({
        success: true,
        data: {
          url: 'http://localhost:3000/api/stream',
          isAuthenticated: true,
        },
      });
    });

    it('should return error when no token exists', () => {
      mockAuthService.getToken.mockReturnValue(null);

      const result = createAuthenticatedSSEUrl('/api/stream');

      expect(result).toEqual({
        success: false,
        error: 'Not authenticated - no token available',
      });
    });

    it('should return error when auth service not initialized', () => {
      setSSEAuthService(null as any, null as any);

      const result = createAuthenticatedSSEUrl('/api/stream');

      expect(result).toEqual({
        success: false,
        error: 'Auth service not initialized',
      });
    });

    it('should handle errors gracefully', () => {
      mockAuthService.getToken.mockImplementation(() => {
        throw new Error('Token access error');
      });

      const result = createAuthenticatedSSEUrl('/api/stream');

      expect(result).toEqual({
        success: false,
        error: 'Failed to create authenticated SSE URL: Token access error',
      });
    });
  });

  describe('validateSSEAuth', () => {
    it('should validate successfully when token exists', () => {
      mockAuthService.getToken.mockReturnValue('valid-token');

      const result = validateSSEAuth();

      expect(result).toEqual({
        success: true,
        data: { isValid: true },
      });
    });

    it('should return error when no token exists', () => {
      mockAuthService.getToken.mockReturnValue(null);

      const result = validateSSEAuth();

      expect(result).toEqual({
        success: false,
        error: 'No authentication token available',
      });
    });

    it('should return error when auth service not initialized', () => {
      setSSEAuthService(null as any, null as any);

      const result = validateSSEAuth();

      expect(result).toEqual({
        success: false,
        error: 'Auth service not initialized',
      });
    });

    it('should handle errors gracefully', () => {
      mockAuthService.getToken.mockImplementation(() => {
        throw new Error('Validation error');
      });

      const result = validateSSEAuth();

      expect(result).toEqual({
        success: false,
        error: 'SSE authentication validation failed: Validation error',
      });
    });
  });

  describe('handleSSEAuthError', () => {
    it('should call centralized logout on 401 error', () => {
      const error = new Error('401: Unauthorized');

      handleSSEAuthError(error);

      expect(mockLogout).toHaveBeenCalled();
      expect(console.error).toHaveBeenCalledWith('SSE authentication error:', error);
    });

    it('should fallback to auth service logout when centralized logout not available', () => {
      setSSEAuthService(mockAuthService, null as any);
      const error = new Error('401: Unauthorized');

      handleSSEAuthError(error);

      expect(mockAuthService.logout).toHaveBeenCalled();
    });

    it('should handle non-401 errors without logout', () => {
      const error = new Error('500: Server Error');

      handleSSEAuthError(error);

      expect(mockLogout).not.toHaveBeenCalled();
      expect(console.warn).toHaveBeenCalledWith('SSE connection error (non-auth):', error);
    });

    it('should handle Event objects', () => {
      const event = new Event('error');

      handleSSEAuthError(event);

      expect(mockLogout).not.toHaveBeenCalled();
      expect(console.warn).toHaveBeenCalledWith('SSE connection error (non-auth):', event);
    });

    it('should handle errors in error handling', () => {
      mockLogout.mockImplementation(() => {
        throw new Error('Logout failed');
      });
      const error = new Error('401: Unauthorized');

      handleSSEAuthError(error);

      expect(console.error).toHaveBeenCalledWith('Error while handling SSE auth error:', expect.any(Error));
    });
  });

  describe('getSSEAuthHeaders', () => {
    it('should return auth headers when token exists', () => {
      mockAuthService.getToken.mockReturnValue('valid-token');

      const headers = getSSEAuthHeaders();

      expect(headers).toEqual({
        'Authorization': 'Bearer valid-token',
        'Content-Type': 'text/event-stream',
        'Cache-Control': 'no-cache',
      });
    });

    it('should return empty object when no token exists', () => {
      mockAuthService.getToken.mockReturnValue(null);

      const headers = getSSEAuthHeaders();

      expect(headers).toEqual({});
    });

    it('should return empty object when auth service not initialized', () => {
      setSSEAuthService(null as any, null as any);

      const headers = getSSEAuthHeaders();

      expect(headers).toEqual({});
    });

    it('should handle errors gracefully', () => {
      mockAuthService.getToken.mockImplementation(() => {
        throw new Error('Header generation error');
      });

      const headers = getSSEAuthHeaders();

      expect(headers).toEqual({});
      expect(console.error).toHaveBeenCalledWith('Error getting SSE auth headers:', expect.any(Error));
    });
  });

  describe('checkSSEPermission', () => {
    it('should return permission granted when token exists', () => {
      mockAuthService.getToken.mockReturnValue('valid-token');

      const result = checkSSEPermission('/api/stream');

      expect(result).toEqual({
        success: true,
        data: { hasPermission: true },
      });
    });

    it('should return error when no token exists', () => {
      mockAuthService.getToken.mockReturnValue(null);

      const result = checkSSEPermission('/api/stream');

      expect(result).toEqual({
        success: false,
        error: 'Authentication required for SSE access',
      });
    });

    it('should return error when auth service not initialized', () => {
      setSSEAuthService(null as any, null as any);

      const result = checkSSEPermission('/api/stream');

      expect(result).toEqual({
        success: false,
        error: 'Auth service not initialized',
      });
    });

    it('should handle errors gracefully', () => {
      mockAuthService.getToken.mockImplementation(() => {
        throw new Error('Permission check error');
      });

      const result = checkSSEPermission('/api/stream');

      expect(result).toEqual({
        success: false,
        error: 'Permission check failed: Permission check error',
      });
    });

    it('should work with different endpoint patterns', () => {
      mockAuthService.getToken.mockReturnValue('valid-token');

      const adminResult = checkSSEPermission('/api/admin/stream');
      const userResult = checkSSEPermission('/api/user/stream');
      const backtestResult = checkSSEPermission('/api/backtests/123/stream');

      expect(adminResult).toEqual({ success: true, data: { hasPermission: true } });
      expect(userResult).toEqual({ success: true, data: { hasPermission: true } });
      expect(backtestResult).toEqual({ success: true, data: { hasPermission: true } });
    });
  });

  describe('setSSEAuthService', () => {
    it('should register new auth service and logout function', () => {
      const newAuthService = {
        getToken: jest.fn().mockReturnValue('new-token'),
        logout: jest.fn(),
      };
      const newLogout = jest.fn();

      setSSEAuthService(newAuthService, newLogout);

      const headers = getSSEAuthHeaders();
      expect(headers['Authorization']).toBe('Bearer new-token');

      const error = new Error('401: Unauthorized');
      handleSSEAuthError(error);
      expect(newLogout).toHaveBeenCalled();
    });
  });

  describe('integration scenarios', () => {
    it('should handle complete SSE authentication flow', () => {
      mockAuthService.getToken.mockReturnValue('valid-token');

      // Check permission
      const permissionResult = checkSSEPermission('/api/backtests/123/stream');
      expect(permissionResult).toEqual({ success: true, data: { hasPermission: true } });

      // Validate auth
      const authResult = validateSSEAuth();
      expect(authResult).toEqual({ success: true, data: { isValid: true } });

      // Create URL
      const urlResult = createAuthenticatedSSEUrl('/api/backtests/123/stream');
      expect(urlResult).toEqual({
        success: true,
        data: {
          url: '/api/backtests/123/stream',
          isAuthenticated: true,
        },
      });

      // Get headers for debugging
      const headers = getSSEAuthHeaders();
      expect(headers['Authorization']).toBe('Bearer valid-token');
    });

    it('should handle authentication failure scenario', () => {
      mockAuthService.getToken.mockReturnValue(null);

      // All operations should fail without token
      expect(checkSSEPermission('/api/stream')).toEqual({
        success: false,
        error: 'Authentication required for SSE access',
      });
      expect(validateSSEAuth()).toEqual({
        success: false,
        error: 'No authentication token available',
      });
      expect(createAuthenticatedSSEUrl('/api/stream')).toEqual({
        success: false,
        error: 'Not authenticated - no token available',
      });
      expect(getSSEAuthHeaders()).toEqual({});
    });
  });
});