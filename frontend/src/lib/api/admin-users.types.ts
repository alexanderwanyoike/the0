export type ManagedUserRole = "admin" | "user";

export interface ManagedUser {
  id: string;
  username: string;
  email: string;
  role: ManagedUserRole;
  isActive: boolean;
  isConfiguredRootAdmin?: boolean;
}

export interface CreateManagedUserInput {
  username?: string;
  email: string;
  password: string;
  role: ManagedUserRole;
}

export type UpdateManagedUserInput = Partial<
  Pick<ManagedUser, "role" | "isActive">
>;
