"use client";

import { FormEvent, useState } from "react";
import { Alert, AlertDescription } from "@/components/ui/alert";
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
  CreateManagedUserInput,
  ManagedUserRole,
} from "@/lib/api/admin-users.types";

const emptyForm = {
  username: "",
  email: "",
  password: "",
  role: "user" as ManagedUserRole,
};

interface CreateUserDialogProps {
  onCreate: (input: CreateManagedUserInput) => Promise<string | null>;
}

export function CreateUserDialog({ onCreate }: CreateUserDialogProps) {
  const [open, setOpen] = useState(false);
  const [form, setForm] = useState(emptyForm);
  const [error, setError] = useState<string | null>(null);
  const [isSubmitting, setIsSubmitting] = useState(false);

  const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    setError(null);
    setIsSubmitting(true);

    try {
      const username = form.username.trim();
      const createError = await onCreate({
        ...form,
        username: username || undefined,
      });

      if (createError) {
        setError(createError);
        return;
      }

      setForm(emptyForm);
      setOpen(false);
    } catch {
      setError("Failed to create user");
    } finally {
      setIsSubmitting(false);
    }
  };

  return (
    <Dialog
      open={open}
      onOpenChange={(nextOpen) => {
        setOpen(nextOpen);
        if (!nextOpen) setError(null);
      }}
    >
      <DialogTrigger asChild>
        <Button>Create user</Button>
      </DialogTrigger>
      <DialogContent>
        <DialogHeader>
          <DialogTitle>Create user</DialogTitle>
        </DialogHeader>
        <form onSubmit={handleSubmit} className="space-y-4">
          {error && (
            <Alert variant="destructive">
              <AlertDescription>{error}</AlertDescription>
            </Alert>
          )}
          <div className="grid gap-2">
            <Label htmlFor="new-email">Email</Label>
            <Input
              id="new-email"
              type="email"
              value={form.email}
              onChange={(event) =>
                setForm({ ...form, email: event.target.value })
              }
              required
            />
          </div>
          <div className="grid gap-2">
            <Label htmlFor="new-username">Username</Label>
            <Input
              id="new-username"
              value={form.username}
              onChange={(event) =>
                setForm({ ...form, username: event.target.value })
              }
            />
          </div>
          <div className="grid gap-2">
            <Label htmlFor="new-password">Initial password</Label>
            <Input
              id="new-password"
              type="password"
              value={form.password}
              onChange={(event) =>
                setForm({ ...form, password: event.target.value })
              }
              required
            />
          </div>
          <div className="grid gap-2">
            <Label id="new-role-label">Role</Label>
            <Select
              value={form.role}
              onValueChange={(role) =>
                setForm({ ...form, role: role as ManagedUserRole })
              }
            >
              <SelectTrigger aria-labelledby="new-role-label">
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="user">User</SelectItem>
                <SelectItem value="admin">Admin</SelectItem>
              </SelectContent>
            </Select>
          </div>
          <Button type="submit" className="w-full" disabled={isSubmitting}>
            Create
          </Button>
        </form>
      </DialogContent>
    </Dialog>
  );
}
