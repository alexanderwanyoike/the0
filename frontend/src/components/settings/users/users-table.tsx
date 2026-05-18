"use client";

import { useState } from "react";
import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
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
import {
  ManagedUser,
  ManagedUserRole,
  UpdateManagedUserInput,
} from "@/lib/api/admin-users.types";

interface UsersTableProps {
  users: ManagedUser[];
  isFetching: boolean;
  onUpdateUser: (id: string, input: UpdateManagedUserInput) => void;
  onResetPassword: (id: string, password: string) => Promise<boolean>;
}

export function UsersTable({
  users,
  isFetching,
  onUpdateUser,
  onResetPassword,
}: UsersTableProps) {
  const [resetPasswords, setResetPasswords] = useState<Record<string, string>>(
    {},
  );

  const handleResetPassword = async (userId: string) => {
    const password = resetPasswords[userId];
    if (!password) return;

    const didReset = await onResetPassword(userId, password);
    if (didReset) {
      setResetPasswords((current) => ({ ...current, [userId]: "" }));
    }
  };

  return (
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
          {isFetching && (
            <TableRow>
              <TableCell colSpan={4} className="text-muted-foreground">
                Loading users...
              </TableCell>
            </TableRow>
          )}
          {!isFetching && users.length === 0 && (
            <TableRow>
              <TableCell colSpan={4} className="text-muted-foreground">
                No users found.
              </TableCell>
            </TableRow>
          )}
          {!isFetching &&
            users.map((managedUser) => (
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
                      onUpdateUser(managedUser.id, {
                        role: role as ManagedUserRole,
                      })
                    }
                  >
                    <SelectTrigger
                      className="w-28"
                      aria-label={`Role for ${managedUser.email}`}
                    >
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
                      onUpdateUser(managedUser.id, {
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
                      disabled={managedUser.isConfiguredRootAdmin}
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
                      disabled={managedUser.isConfiguredRootAdmin}
                      onClick={() => handleResetPassword(managedUser.id)}
                    >
                      Reset
                    </Button>
                  </div>
                </TableCell>
              </TableRow>
            ))}
        </TableBody>
      </Table>
    </div>
  );
}
