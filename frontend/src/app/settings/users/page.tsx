"use client";

import { useCallback, useEffect, useMemo, useState } from "react";
import { useRouter } from "next/navigation";
import { Alert, AlertDescription } from "@/components/ui/alert";
import { useAuth } from "@/contexts/auth-context";
import {
  createAdminUser,
  listAdminUsers,
  resetAdminUserPassword,
  updateAdminUser,
} from "@/lib/api/admin-users";
import { CreateUserDialog } from "@/components/settings/users/create-user-dialog";
import { UsersTable } from "@/components/settings/users/users-table";
import {
  CreateManagedUserInput,
  ManagedUser,
  UpdateManagedUserInput,
} from "@/lib/api/admin-users.types";

export default function UserManagementPage() {
  const { user, loading } = useAuth();
  const router = useRouter();
  const [users, setUsers] = useState<ManagedUser[]>([]);
  const [error, setError] = useState<string | null>(null);
  const [isFetching, setIsFetching] = useState(true);

  const sortedUsers = useMemo(
    () => [...users].sort((a, b) => a.email.localeCompare(b.email)),
    [users],
  );

  const loadUsers = useCallback(async () => {
    setIsFetching(true);
    setError(null);
    const result = await listAdminUsers();
    if (result.success) {
      setUsers(result.data || []);
    } else {
      setError(result.error);
    }
    setIsFetching(false);
  }, []);

  useEffect(() => {
    if (loading) return;
    if (!user) {
      router.replace("/login");
      return;
    }
    if (user.role !== "admin") {
      router.replace("/settings/profile");
      return;
    }
    loadUsers();
  }, [loadUsers, loading, router, user]);

  const handleUpdateUser = async (
    id: string,
    input: UpdateManagedUserInput,
  ) => {
    setError(null);
    const result = await updateAdminUser(id, input);
    if (!result.success) {
      setError(result.error);
      return;
    }

    setUsers((current) =>
      current.map((managedUser) =>
        managedUser.id === id ? result.data : managedUser,
      ),
    );
  };

  const handleCreateUser = async (
    input: CreateManagedUserInput,
  ): Promise<string | null> => {
    setError(null);
    const result = await createAdminUser(input);
    if (!result.success) {
      return result.error;
    }

    setUsers((current) => [...current, result.data]);
    return null;
  };

  const handleResetPassword = async (
    id: string,
    password: string,
  ): Promise<boolean> => {
    setError(null);
    const result = await resetAdminUserPassword(id, password);
    if (!result.success) {
      setError(result.error);
      return false;
    }
    return true;
  };

  if (loading || !user || user.role !== "admin") {
    return null;
  }

  return (
    <div className="space-y-6">
      <div className="flex flex-col gap-3 sm:flex-row sm:items-center sm:justify-between">
        <div>
          <h2 className="text-lg font-medium">Users</h2>
          <p className="mt-1 text-sm text-muted-foreground">
            Create accounts, reset passwords, and manage admin access.
          </p>
        </div>
        <CreateUserDialog onCreate={handleCreateUser} />
      </div>

      {error && (
        <Alert variant="destructive">
          <AlertDescription>{error}</AlertDescription>
        </Alert>
      )}

      <UsersTable
        users={sortedUsers}
        isFetching={isFetching}
        onUpdateUser={handleUpdateUser}
        onResetPassword={handleResetPassword}
      />
    </div>
  );
}
