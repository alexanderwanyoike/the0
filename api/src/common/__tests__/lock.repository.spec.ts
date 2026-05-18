import { Logger } from "@nestjs/common";
import { AdminMutationLockRepository } from "@/user/admin-mutation-lock.repository";
import { getDatabase, getDatabaseConfig } from "@/database/connection";

jest.mock("drizzle-orm", () => ({
  and: jest.fn((...conditions) => ({ type: "and", conditions })),
  eq: jest.fn((column, value) => ({ type: "eq", column, value })),
}));

jest.mock("@/database/connection", () => ({
  getDatabase: jest.fn(),
  getDatabaseConfig: jest.fn(),
}));

interface FakeLockRecord {
  id: string;
  lockedAt: number;
}

type DeleteCondition = {
  type: "and";
  conditions: Array<{ type: "eq"; column: unknown; value: unknown }>;
};

function conditionValues(condition: DeleteCondition) {
  return condition.conditions.map((entry) => entry.value);
}

function createLockDb() {
  const insertValues = jest.fn();
  const selectWhere = jest.fn();
  const deleteWhere = jest.fn().mockResolvedValue(undefined);

  const db = {
    insert: jest.fn(() => ({
      values: insertValues,
    })),
    select: jest.fn(() => ({
      from: jest.fn(() => ({
        where: selectWhere,
      })),
    })),
    delete: jest.fn(() => ({
      where: deleteWhere,
    })),
  };

  (getDatabase as jest.Mock).mockReturnValue(db);
  (getDatabaseConfig as jest.Mock).mockReturnValue({ type: "sqlite" });

  return {
    db,
    insertValues,
    selectWhere,
    deleteWhere,
  };
}

function flushPromises(): Promise<void> {
  return new Promise((resolve) => setImmediate(resolve));
}

function uniqueConstraintError() {
  return Object.assign(new Error("duplicate lock"), {
    code: "SQLITE_CONSTRAINT_PRIMARYKEY",
  });
}

describe("Lock repositories", () => {
  let dateNow: jest.SpyInstance<number, []>;

  beforeEach(() => {
    jest.clearAllMocks();
    dateNow = jest.spyOn(Date, "now");
  });

  afterEach(() => {
    dateNow.mockRestore();
  });

  it("removes only the observed stale admin mutation lock before acquiring its own lock", async () => {
    const staleLockedAt = 2_000;
    const acquiredLockedAt = 500_000;
    const { insertValues, selectWhere, deleteWhere } = createLockDb();
    insertValues
      .mockRejectedValueOnce(uniqueConstraintError())
      .mockResolvedValueOnce(undefined);
    selectWhere.mockResolvedValueOnce([
      { id: "admin-users", lockedAt: staleLockedAt },
    ] satisfies FakeLockRecord[]);
    dateNow
      .mockReturnValueOnce(400_000)
      .mockReturnValueOnce(410_001)
      .mockReturnValueOnce(acquiredLockedAt);

    const callback = jest.fn().mockResolvedValue("updated");
    const result = await new AdminMutationLockRepository().withLock(callback);

    expect(result).toBe("updated");
    expect(callback).toHaveBeenCalledTimes(1);
    expect(deleteWhere).toHaveBeenCalledTimes(2);
    expect(conditionValues(deleteWhere.mock.calls[0][0])).toContain(
      staleLockedAt,
    );
    expect(conditionValues(deleteWhere.mock.calls[1][0])).toContain(
      acquiredLockedAt,
    );
  });

  it("runs lock callbacks one at a time within the same process", async () => {
    const { insertValues } = createLockDb();
    insertValues.mockResolvedValue(undefined);
    dateNow.mockReturnValueOnce(100).mockReturnValueOnce(200);

    const repository = new AdminMutationLockRepository();
    const events: string[] = [];
    let releaseFirstLock: () => void = () => undefined;
    const firstCallbackStarted = new Promise<void>((resolve) => {
      releaseFirstLock = resolve;
    });

    const first = repository.withLock(async () => {
      events.push("first started");
      await firstCallbackStarted;
      events.push("first finished");
      return "first";
    });

    await flushPromises();

    const second = repository.withLock(async () => {
      events.push("second started");
      return "second";
    });

    await flushPromises();

    expect(events).toEqual(["first started"]);

    releaseFirstLock();

    await expect(Promise.all([first, second])).resolves.toEqual([
      "first",
      "second",
    ]);
    expect(events).toEqual([
      "first started",
      "first finished",
      "second started",
    ]);
  });

  it("does not treat non-constraint insert errors as lock contention", async () => {
    const { insertValues, selectWhere } = createLockDb();
    insertValues.mockRejectedValueOnce(
      Object.assign(new Error("integer out of range"), {
        code: "22003",
      }),
    );

    await expect(
      new AdminMutationLockRepository().withLock(jest.fn()),
    ).rejects.toThrow("integer out of range");
    expect(selectWhere).not.toHaveBeenCalled();
  });

  it("preserves callback errors and logs admin mutation lock release failures", async () => {
    const callbackError = new Error("callback failed");
    const releaseError = new Error("release failed");
    const loggerError = jest
      .spyOn(Logger.prototype, "error")
      .mockImplementation();
    const { insertValues, deleteWhere } = createLockDb();
    insertValues.mockResolvedValueOnce(undefined);
    deleteWhere.mockRejectedValueOnce(releaseError);
    dateNow.mockReturnValueOnce(100);

    await expect(
      new AdminMutationLockRepository().withLock(async () => {
        throw callbackError;
      }),
    ).rejects.toThrow("callback failed");

    expect(loggerError).toHaveBeenCalledWith(
      "admin mutation lock: failed to release lock after callback error",
      releaseError.stack,
    );
    loggerError.mockRestore();
  });

  it("surfaces admin mutation lock release errors after successful callbacks", async () => {
    const releaseError = new Error("release failed");
    const loggerError = jest
      .spyOn(Logger.prototype, "error")
      .mockImplementation();
    const { insertValues, deleteWhere } = createLockDb();
    insertValues.mockResolvedValueOnce(undefined);
    deleteWhere.mockRejectedValueOnce(releaseError);
    dateNow.mockReturnValueOnce(100);

    await expect(
      new AdminMutationLockRepository().withLock(async () => "updated"),
    ).rejects.toThrow("release failed");
    expect(loggerError).toHaveBeenCalledWith(
      "admin mutation lock: failed to release lock",
      releaseError.stack,
    );
    loggerError.mockRestore();
  });
});
