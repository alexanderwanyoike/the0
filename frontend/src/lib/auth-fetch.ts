"use client";

export interface AuthFetchOptions extends RequestInit {
  skipTokenRefresh?: boolean;
}

// Global auth service reference - will be set by auth context
let globalAuthService: any = null;
let globalLogout: (() => Promise<void>) | null = null;

// Function to set the auth service from the context
export const setAuthFetchService = (
  authService: any,
  logoutFn: () => Promise<void>,
) => {
  globalAuthService = authService;
  globalLogout = logoutFn;
};

export async function authFetch(
  url: string,
  options: AuthFetchOptions = {},
): Promise<Response> {
  if (!globalAuthService) {
    throw new Error("Auth service not initialized");
  }

  const token = globalAuthService.getToken();

  if (!token) {
    throw new Error("Not authenticated");
  }

  const { skipTokenRefresh, ...fetchOptions } = options;

  const requestOptions: RequestInit = {
    ...fetchOptions,
    headers: {
      "Content-Type": "application/json",
      ...fetchOptions.headers,
      Authorization: `Bearer ${token}`,
    },
  };

  const response = await fetch(url, requestOptions);

  // If 401 and not skipping refresh, use centralized logout
  if (response.status === 401 && !skipTokenRefresh) {
    if (globalLogout) {
      await globalLogout();
    } else if (globalAuthService) {
      globalAuthService.logout();
    }
    throw new Error("Authentication failed");
  }

  return response;
}
