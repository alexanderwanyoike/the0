"use client";

import { authFetch } from "@/lib/auth-fetch";
import {
  CreateManagedUserInput,
  ManagedUser,
  UpdateManagedUserInput,
} from "./admin-users.types";

type ApiResult<T> =
  | { success: true; data: T }
  | { success: false; error: string };

async function responseError(
  response: Response,
  fallback: string,
): Promise<string> {
  try {
    const body = await response.json();
    return body.message || fallback;
  } catch {
    return fallback;
  }
}

async function readData<T>(
  response: Response,
  fallback: string,
): Promise<ApiResult<T>> {
  if (!response.ok) {
    return { success: false, error: await responseError(response, fallback) };
  }

  const body = await response.json();
  return { success: true, data: body.data };
}

export async function listAdminUsers(): Promise<ApiResult<ManagedUser[]>> {
  try {
    const response = await authFetch("/api/admin/users");
    return readData<ManagedUser[]>(response, "Failed to load users");
  } catch (error) {
    return {
      success: false,
      error: error instanceof Error ? error.message : "Failed to load users",
    };
  }
}

export async function createAdminUser(
  input: CreateManagedUserInput,
): Promise<ApiResult<ManagedUser>> {
  try {
    const response = await authFetch("/api/admin/users", {
      method: "POST",
      body: JSON.stringify(input),
    });
    return readData<ManagedUser>(response, "Failed to create user");
  } catch {
    return { success: false, error: "Failed to create user" };
  }
}

export async function updateAdminUser(
  id: string,
  input: UpdateManagedUserInput,
): Promise<ApiResult<ManagedUser>> {
  try {
    const response = await authFetch(`/api/admin/users/${id}`, {
      method: "PATCH",
      body: JSON.stringify(input),
    });
    return readData<ManagedUser>(response, "Failed to update user");
  } catch {
    return { success: false, error: "Failed to update user" };
  }
}

export async function resetAdminUserPassword(
  id: string,
  password: string,
): Promise<ApiResult<ManagedUser>> {
  try {
    const response = await authFetch(`/api/admin/users/${id}/reset-password`, {
      method: "POST",
      body: JSON.stringify({ password }),
    });
    return readData<ManagedUser>(response, "Failed to reset password");
  } catch {
    return { success: false, error: "Failed to reset password" };
  }
}
