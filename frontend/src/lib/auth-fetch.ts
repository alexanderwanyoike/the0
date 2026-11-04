"use client";

import { Failure, Ok, Result } from "@/lib/result";

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

class AuthenticationError extends Error {}

export async function authFetch(
  url: string,
  options: AuthFetchOptions = {},
): Promise<Response> {
  if (!globalAuthService) {
    throw new Error("Auth service not initialized");
  }

  const token = globalAuthService.getToken();

  if (!token) {
    throw new AuthenticationError("Not authenticated");
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
    throw new AuthenticationError("Authentication failed");
  }

  return response;
}

export interface ApiError {
  message: string;
  statusCode: number;
}

export interface ApiRequestOptions {
  method?: string;
  /** Sent as JSON. */
  body?: unknown;
  timeoutMs?: number;
}

/** A JSON call to the api through authFetch, as a Result. */
export async function apiRequest<T>(
  url: string,
  { method = "GET", body, timeoutMs }: ApiRequestOptions = {},
): Promise<Result<T, ApiError>> {
  const controller = new AbortController();
  const timeout =
    timeoutMs === undefined
      ? undefined
      : setTimeout(() => controller.abort(), timeoutMs);

  try {
    const response = await authFetch(url, {
      method,
      body: body === undefined ? undefined : JSON.stringify(body),
      signal: controller.signal,
    });
    const data = await readBody(response);
    if (!response.ok) {
      return Failure({
        message: errorMessage(data) || response.statusText || "Request failed",
        statusCode: response.status,
      });
    }
    return Ok(data as T);
  } catch (error) {
    if (error instanceof AuthenticationError) {
      return Failure({ message: error.message, statusCode: 401 });
    }
    if (controller.signal.aborted) {
      return Failure({ message: "Request timed out", statusCode: 504 });
    }
    return Failure({
      message: "No response received from server",
      statusCode: 500,
    });
  } finally {
    clearTimeout(timeout);
  }
}

async function readBody(response: Response): Promise<unknown> {
  const text = await response.text();
  if (!text) return null;
  try {
    return JSON.parse(text);
  } catch {
    return text;
  }
}

// The api answers errors as { message }, where validation errors carry one
// message per failed rule.
function errorMessage(data: unknown): string | undefined {
  if (typeof data !== "object" || data === null) return undefined;
  const { message } = data as { message?: unknown };
  if (Array.isArray(message)) return message.join(", ");
  return typeof message === "string" ? message : undefined;
}
