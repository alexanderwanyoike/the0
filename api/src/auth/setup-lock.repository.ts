import { Injectable } from "@nestjs/common";
import { LockRepository } from "@/common/lock.repository";
import { getDatabaseConfig } from "@/database/connection";
import {
  setupLocksTable,
  setupLocksTableSqlite,
} from "@/database/schema/users";

const SETUP_LOCK_ID = "first-admin";
const SETUP_LOCK_STALE_MS = 5 * 60 * 1000;

type LockResult<T> =
  | { acquired: true; value: T }
  | { acquired: false; value: null };

@Injectable()
export class SetupLockRepository extends LockRepository {
  protected readonly lockId = SETUP_LOCK_ID;
  protected readonly staleMs = SETUP_LOCK_STALE_MS;

  protected getLockTable() {
    const config = getDatabaseConfig();
    return config.type === "sqlite" ? setupLocksTableSqlite : setupLocksTable;
  }

  async withLock<T>(callback: () => Promise<T>): Promise<LockResult<T>> {
    return this.withLocalLock(async (): Promise<LockResult<T>> => {
      let lockedAt: number | null = null;
      for (let attempt = 0; attempt < 2; attempt += 1) {
        const lock = await this.tryAcquire();
        if (lock.status === "acquired") {
          lockedAt = lock.lockedAt;
          break;
        }
        if (lock.status === "busy") {
          return { acquired: false, value: null };
        }
      }

      if (lockedAt === null) {
        return { acquired: false, value: null };
      }

      try {
        return { acquired: true, value: await callback() };
      } finally {
        await this.release(lockedAt).catch((): void => undefined);
      }
    });
  }
}
