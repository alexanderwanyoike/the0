import { Injectable, ServiceUnavailableException } from "@nestjs/common";
import { LockRepository, wait } from "@/common/lock.repository";
import { getDatabaseConfig } from "@/database/connection";
import {
  adminMutationLocksTable,
  adminMutationLocksTableSqlite,
} from "@/database/schema/users";

const ADMIN_MUTATION_LOCK_ID = "admin-users";
const ADMIN_MUTATION_LOCK_STALE_MS = 5 * 60 * 1000;

@Injectable()
export class AdminMutationLockRepository extends LockRepository {
  protected readonly lockId = ADMIN_MUTATION_LOCK_ID;
  protected readonly staleMs = ADMIN_MUTATION_LOCK_STALE_MS;

  protected getLockTable() {
    const config = getDatabaseConfig();
    return config.type === "sqlite"
      ? adminMutationLocksTableSqlite
      : adminMutationLocksTable;
  }

  async withLock<T>(callback: () => Promise<T>): Promise<T> {
    return this.withLocalLock(async () => {
      const lockedAt = await this.acquireWithRetries();
      return this.runWithRelease(lockedAt, callback, "admin mutation lock");
    });
  }

  private async acquireWithRetries(): Promise<number> {
    for (let attempt = 0; attempt < 50; attempt += 1) {
      const lock = await this.tryAcquire();
      if (lock.status === "acquired") {
        return lock.lockedAt;
      }
      if (lock.status === "busy") {
        await wait(50);
      }
    }

    throw new ServiceUnavailableException(
      "Admin user management is temporarily locked",
    );
  }
}
