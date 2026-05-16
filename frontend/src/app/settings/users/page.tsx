"use client";

import { FormEvent, useEffect, useMemo, useState } from "react";
import { useRouter } from "next/navigation";
import { Alert, AlertDescription } from "@/components/ui/alert";
import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from "@/components/ui/dialog";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import { authFetch } from "@/lib/auth-fetch";
import { useAuth } from "@/contexts/auth-context";

interface ManagedUser {
  id: string;
  username: string;
  email: string;
  role: "admin" | "user";
  isActive: boolean;
}

const emptyCreateForm = {
  username: "",
  email: "",
  password: "",
  role: "user" as "admin" | "user",
};

async function readErrorMessage(
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

export default function UserManagementPage() {
  const { user, loading } = useAuth();
  const router = useRouter();
  const [users, setUsers] = useState<ManagedUser[]>([]);
  const [error, setError] = useState<string | null>(null);
  const [isFetching, setIsFetching] = useState(true);
  const [createOpen, setCreateOpen] = useState(false);
  const [createForm, setCreateForm] = useState(emptyCreateForm);
  const [resetPasswords, setResetPasswords] = useState<Record<string, string>>(
    {},
  );

  const sortedUsers = useMemo(
    () => [...users].sort((a, b) => a.email.localeCompare(b.email)),
    [users],
  );

  const loadUsers = async () => {
    setIsFetching(true);
    setError(null);
    try {
      const response = await authFetch("/api/admin/users");
      const body = await response.json();
      if (!response.ok) {
        throw new Error(body.message || "Failed to load users");
      }
      setUsers(body.data || []);
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to load users");
    } finally {
      setIsFetching(false);
    }
  };

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
  }, [loading, router, user]);

  const updateUser = async (id: string, body: Partial<ManagedUser>) => {
    setError(null);
    try {
      const response = await authFetch(`/api/admin/users/${id}`, {
        method: "PATCH",
        body: JSON.stringify(body),
      });
      if (!response.ok) {
        setError(await readErrorMessage(response, "Failed to update user"));
        return;
      }
      const data = await response.json();
      setUsers((current) =>
        current.map((managedUser) =>
          managedUser.id === id ? data.data : managedUser,
        ),
      );
    } catch {
      setError("Failed to update user");
    }
  };

  const createUser = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    setError(null);
    const username = createForm.username.trim();
    if (!username) {
      setError("Username is required");
      return;
    }

    try {
      const response = await authFetch("/api/admin/users", {
        method: "POST",
        body: JSON.stringify({ ...createForm, username }),
      });
      if (!response.ok) {
        setError(await readErrorMessage(response, "Failed to create user"));
        return;
      }
      const body = await response.json();
      setUsers((current) => [...current, body.data]);
      setCreateForm(emptyCreateForm);
      setCreateOpen(false);
    } catch {
      setError("Failed to create user");
    }
  };

  const resetPassword = async (id: string) => {
    const password = resetPasswords[id];
    if (!password) return;
    setError(null);
    try {
      const response = await authFetch(
        `/api/admin/users/${id}/reset-password`,
        {
          method: "POST",
          body: JSON.stringify({ password }),
        },
      );
      if (!response.ok) {
        setError(await readErrorMessage(response, "Failed to reset password"));
        return;
      }
      setResetPasswords((current) => ({ ...current, [id]: "" }));
    } catch {
      setError("Failed to reset password");
    }
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
        <Dialog open={createOpen} onOpenChange={setCreateOpen}>
          <DialogTrigger asChild>
            <Button>Create user</Button>
          </DialogTrigger>
          <DialogContent>
            <DialogHeader>
              <DialogTitle>Create user</DialogTitle>
            </DialogHeader>
            <form onSubmit={createUser} className="space-y-4">
              <div className="grid gap-2">
                <Label htmlFor="new-email">Email</Label>
                <Input
                  id="new-email"
                  type="email"
                  value={createForm.email}
                  onChange={(event) =>
                    setCreateForm({ ...createForm, email: event.target.value })
                  }
                  required
                />
              </div>
              <div className="grid gap-2">
                <Label htmlFor="new-username">Username</Label>
                <Input
                  id="new-username"
                  value={createForm.username}
                  onChange={(event) =>
                    setCreateForm({
                      ...createForm,
                      username: event.target.value,
                    })
                  }
                  required
                />
              </div>
              <div className="grid gap-2">
                <Label htmlFor="new-password">Initial password</Label>
                <Input
                  id="new-password"
                  type="password"
                  value={createForm.password}
                  onChange={(event) =>
                    setCreateForm({
                      ...createForm,
                      password: event.target.value,
                    })
                  }
                  required
                />
              </div>
              <div className="grid gap-2">
                <Label>Role</Label>
                <Select
                  value={createForm.role}
                  onValueChange={(role) =>
                    setCreateForm({
                      ...createForm,
                      role: role as "admin" | "user",
                    })
                  }
                >
                  <SelectTrigger>
                    <SelectValue />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="user">User</SelectItem>
                    <SelectItem value="admin">Admin</SelectItem>
                  </SelectContent>
                </Select>
              </div>
              <Button type="submit" className="w-full">
                Create
              </Button>
            </form>
          </DialogContent>
        </Dialog>
      </div>

      {error && (
        <Alert variant="destructive">
          <AlertDescription>{error}</AlertDescription>
        </Alert>
      )}

      <div className="rounded-md border">
        <Table>
          <TableHeader>
            <TableRow>
              <TableHead>User</TableHead>
              <TableHead>Role</TableHead>
              <TableHead>Status</TableHead>
              <TableHead className="w-[260px]">Password</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {isFetching ? (
              <TableRow>
                <TableCell colSpan={4} className="text-muted-foreground">
                  Loading users...
                </TableCell>
              </TableRow>
            ) : (
              sortedUsers.map((managedUser) => (
                <TableRow key={managedUser.id}>
                  <TableCell>
                    <div className="font-medium">{managedUser.username}</div>
                    <div className="text-sm text-muted-foreground">
                      {managedUser.email}
                    </div>
                  </TableCell>
                  <TableCell>
                    <Select
                      value={managedUser.role}
                      onValueChange={(role) =>
                        updateUser(managedUser.id, {
                          role: role as "admin" | "user",
                        })
                      }
                    >
                      <SelectTrigger className="w-28">
                        <SelectValue />
                      </SelectTrigger>
                      <SelectContent>
                        <SelectItem value="user">User</SelectItem>
                        <SelectItem value="admin">Admin</SelectItem>
                      </SelectContent>
                    </Select>
                  </TableCell>
                  <TableCell>
                    <Button
                      variant="ghost"
                      size="sm"
                      onClick={() =>
                        updateUser(managedUser.id, {
                          isActive: !managedUser.isActive,
                        })
                      }
                    >
                      <Badge
                        variant={managedUser.isActive ? "default" : "secondary"}
                      >
                        {managedUser.isActive ? "Active" : "Inactive"}
                      </Badge>
                    </Button>
                  </TableCell>
                  <TableCell>
                    <div className="flex gap-2">
                      <Label
                        htmlFor={`reset-password-${managedUser.id}`}
                        className="sr-only"
                      >
                        New password for {managedUser.email}
                      </Label>
                      <Input
                        id={`reset-password-${managedUser.id}`}
                        type="password"
                        placeholder="New password"
                        aria-label={`New password for ${managedUser.email}`}
                        value={resetPasswords[managedUser.id] || ""}
                        onChange={(event) =>
                          setResetPasswords((current) => ({
                            ...current,
                            [managedUser.id]: event.target.value,
                          }))
                        }
                      />
                      <Button
                        variant="outline"
                        aria-label={`Reset password for ${managedUser.email}`}
                        onClick={() => resetPassword(managedUser.id)}
                      >
                        Reset
                      </Button>
                    </div>
                  </TableCell>
                </TableRow>
              ))
            )}
          </TableBody>
        </Table>
      </div>
    </div>
  );
}
