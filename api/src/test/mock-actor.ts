import { Actor } from "@/auth/actor";

const defaults: Actor = {
  userId: "test-user-id",
  via: "jwt",
};

export function mockActor(overrides: Partial<Actor> = {}): Actor {
  return { ...defaults, ...overrides };
}
