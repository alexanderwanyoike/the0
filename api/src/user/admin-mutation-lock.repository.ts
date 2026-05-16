import { Injectable, ServiceUnavailableException } from "@nestjs/common";
import { eq } from "drizzle-orm";
import { getDatabase, getDatabaseConfig } from "@/database/connection";
import {
  type AdminMutationLock,
  adminMutationLocksTable,
  adminMutationLocksTableSqlite,
} from "@/database/schema/users";

const ADMIN_MUTATION_LOCK_ID = "admin-users";
const ADMIN_MUTATION_LOCK_STALE_MS = 5 * 60 * 1000;

@Injectable()
export class AdminMutationLockRepository {
  private localLock = Promise.resolve();

  private getAdminMutationLockTable() {
    const config = getDatabaseConfig();
    return config.type === "sqlite"
      ? adminMutationLocksTableSqlite
      : adminMutationLocksTable;
  }

  async withLock<T>(callback: () => Promise<T>): Promise<T> {
    const previousLock = this.localLock;
    let releaseLocalLock: () => void = () => undefined;
    this.localLock = new Promise<void>((resolve) => {
      releaseLocalLock = resolve;
    });

    await previousLock;
    try {
      await this.acquire();
      try {
        return await callback();
      } finally {
        await this.release().catch((): void => undefined);
      }
    } finally {
      releaseLocalLock();
    }
  }

  private async acquire(): Promise<void> {
    const db = getDatabase();
    const lockTable = this.getAdminMutationLockTable();

    for (let attempt = 0; attempt < 50; attempt += 1) {
      try {
        await db
          .insert(lockTable)
          .values({ id: ADMIN_MUTATION_LOCK_ID, lockedAt: Date.now() });
        return;
      } catch {
        const locks = (await db
          .select()
          .from(lockTable)
          .where(
            eq(lockTable.id, ADMIN_MUTATION_LOCK_ID),
          )) as AdminMutationLock[];
        const existingLock = locks[0];
        if (
          existingLock &&
          Date.now() - Number(existingLock.lockedAt) >
            ADMIN_MUTATION_LOCK_STALE_MS
        ) {
          await this.release();
          continue;
        }

        await wait(50);
      }
    }

    throw new ServiceUnavailableException(
      "Admin user management is temporarily locked",
    );
  }

  private async release(): Promise<void> {
    const db = getDatabase();
    const lockTable = this.getAdminMutationLockTable();
    await db.delete(lockTable).where(eq(lockTable.id, ADMIN_MUTATION_LOCK_ID));
  }
}

function wait(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
