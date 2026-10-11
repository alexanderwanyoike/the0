import { PGlite } from "@electric-sql/pglite";
import { drizzle, PgliteDatabase } from "drizzle-orm/pglite";
import { readFileSync } from "fs";
import { join } from "path";
import type { TableRegistry } from "@/database/connection";
import {
  usersTable,
  adminMutationLocksTable,
  apiKeysTable,
} from "@/database/schema/users";
import { customBotsTable } from "@/database/schema/custom-bots";
import { botsTable } from "@/database/schema/bots";

/**
 * A real Postgres (PGlite, in-process) for tests that need transactions
 * and SQL to behave as in production. Use it in place of the global
 * database mock:
 *
 *   jest.mock(
 *     "@/database/connection",
 *     () => jest.requireActual("@/test/test-database").connection,
 *   );
 */

const MIGRATIONS_DIR = join(__dirname, "..", "database", "migrations");

const tables: TableRegistry = {
  users: usersTable,
  adminMutationLocks: adminMutationLocksTable,
  apiKeys: apiKeysTable,
  customBots: customBotsTable,
  bots: botsTable,
};

let client: PGlite | null = null;
let db: PgliteDatabase | null = null;

export const connection = {
  getDatabase: () => {
    if (!db) throw new Error("Call startTestDatabase() first");
    return db;
  },
  getTables: () => tables,
  getDatabaseConfig: () => ({ type: "postgresql", url: "pglite://memory" }),
};

/**
 * Applies the journal's migrations in order, as drizzle's migrator does.
 * exec() is used because PGlite will not run several statements as one
 * prepared query, and some migration files have no statement breakpoints.
 */
async function migrate(pg: PGlite): Promise<void> {
  const journal = JSON.parse(
    readFileSync(join(MIGRATIONS_DIR, "meta", "_journal.json"), "utf8"),
  ) as { entries: { tag: string }[] };
  for (const { tag } of journal.entries) {
    await pg.exec(readFileSync(join(MIGRATIONS_DIR, `${tag}.sql`), "utf8"));
  }
}

export async function startTestDatabase(): Promise<PgliteDatabase> {
  client = new PGlite();
  await migrate(client);
  db = drizzle(client);
  return db;
}

export async function stopTestDatabase(): Promise<void> {
  await client?.close();
  client = null;
  db = null;
}

/** Empties every table, restarting id sequences. */
export async function resetTestDatabase(): Promise<void> {
  if (!client) throw new Error("Call startTestDatabase() first");
  const { rows } = await client.query<{ tablename: string }>(
    "SELECT tablename FROM pg_tables WHERE schemaname = 'public'",
  );
  const names = rows.map(({ tablename }) => `"${tablename}"`).join(", ");
  await client.exec(`TRUNCATE ${names} RESTART IDENTITY CASCADE`);
}
