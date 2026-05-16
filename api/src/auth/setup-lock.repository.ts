import { Injectable } from "@nestjs/common";
import { eq } from "drizzle-orm";
import { isConnectionError } from "@/common/database-errors";
import { getDatabase, getDatabaseConfig } from "@/database/connection";
import {
  type SetupLock,
  setupLocksTable,
  setupLocksTableSqlite,
} from "@/database/schema/users";

const SETUP_LOCK_ID = "first-admin";
const SETUP_LOCK_STALE_MS = 5 * 60 * 1000;

type LockResult<T> =
  | { acquired: true; value: T }
  | { acquired: false; value: null };

@Injectable()
export class SetupLockRepository {
  private localLock = Promise.resolve();

  private getSetupLockTable() {
    const config = getDatabaseConfig();
    return config.type === "sqlite" ? setupLocksTableSqlite : setupLocksTable;
  }

  async withLock<T>(callback: () => Promise<T>): Promise<LockResult<T>> {
    const previousLock = this.localLock;
    let releaseLocalLock: () => void = () => undefined;
    this.localLock = new Promise<void>((resolve) => {
      releaseLocalLock = resolve;
    });

    await previousLock;
    let setupLockClaimed = false;
    try {
      setupLockClaimed = await this.acquire();
      if (!setupLockClaimed) {
        return { acquired: false, value: null };
      }

      return { acquired: true, value: await callback() };
    } finally {
      if (setupLockClaimed) {
        await this.release().catch((): void => undefined);
      }
      releaseLocalLock();
    }
  }

  private async acquire(): Promise<boolean> {
    const db = getDatabase();
    const setupLockTable = this.getSetupLockTable();
    try {
      await db
        .insert(setupLockTable)
        .values({ id: SETUP_LOCK_ID, lockedAt: Date.now() });
      return true;
    } catch (error) {
      if (isConnectionError(error)) {
        throw error;
      }

      const locks = (await db
        .select()
        .from(setupLockTable)
        .where(eq(setupLockTable.id, SETUP_LOCK_ID))) as SetupLock[];
      const existingLock = locks[0];
      if (
        existingLock &&
        Date.now() - Number(existingLock.lockedAt) > SETUP_LOCK_STALE_MS
      ) {
        await this.release();
        return this.acquire();
      }

      return false;
    }
  }

  private async release(): Promise<void> {
    const db = getDatabase();
    const setupLockTable = this.getSetupLockTable();
    await db.delete(setupLockTable).where(eq(setupLockTable.id, SETUP_LOCK_ID));
  }
}
