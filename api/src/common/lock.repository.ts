import { and, eq, type AnyColumn } from "drizzle-orm";
import {
  isConnectionError,
  isUniqueConstraintError,
} from "@/common/database-errors";
import { getDatabase } from "@/database/connection";

export interface LockTable {
  id: AnyColumn;
  lockedAt: AnyColumn;
}

type AcquireResult =
  | { status: "acquired"; lockedAt: number }
  | { status: "busy" }
  | { status: "stale-released" };

interface LockRecord {
  id: string;
  lockedAt: number;
}

export abstract class LockRepository {
  private localLock = Promise.resolve();

  protected abstract readonly lockId: string;
  protected abstract readonly staleMs: number;

  protected abstract getLockTable(): LockTable;

  protected async withLocalLock<T>(callback: () => Promise<T>): Promise<T> {
    const previousLock = this.localLock;
    let releaseLocalLock: () => void = () => undefined;
    this.localLock = new Promise<void>((resolve) => {
      releaseLocalLock = resolve;
    });

    await previousLock;
    try {
      return await callback();
    } finally {
      releaseLocalLock();
    }
  }

  protected async tryAcquire(): Promise<AcquireResult> {
    const db = getDatabase();
    const lockTable = this.getLockTable();
    const lockedAt = Date.now();

    try {
      await db.insert(lockTable).values({ id: this.lockId, lockedAt });
      return { status: "acquired", lockedAt };
    } catch (error) {
      if (isConnectionError(error)) {
        throw error;
      }
      if (!isUniqueConstraintError(error)) {
        throw error;
      }

      const locks = (await db
        .select()
        .from(lockTable)
        .where(eq(lockTable.id, this.lockId))) as LockRecord[];
      const existingLock = locks[0];

      if (
        existingLock &&
        Date.now() - Number(existingLock.lockedAt) > this.staleMs
      ) {
        await this.releaseIfMatches(existingLock.lockedAt);
        return { status: "stale-released" };
      }

      return { status: "busy" };
    }
  }

  protected async release(lockedAt: number): Promise<void> {
    await this.releaseIfMatches(lockedAt);
  }

  private async releaseIfMatches(lockedAt: number): Promise<void> {
    const db = getDatabase();
    const lockTable = this.getLockTable();

    await db
      .delete(lockTable)
      .where(
        and(eq(lockTable.id, this.lockId), eq(lockTable.lockedAt, lockedAt)),
      );
  }
}

export function wait(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
