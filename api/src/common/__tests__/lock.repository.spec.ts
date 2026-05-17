import { SetupLockRepository } from "@/auth/setup-lock.repository";
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

describe("Lock repositories", () => {
  let dateNow: jest.SpyInstance<number, []>;

  beforeEach(() => {
    jest.clearAllMocks();
    dateNow = jest.spyOn(Date, "now");
  });

  afterEach(() => {
    dateNow.mockRestore();
  });

  it("does not run setup callback when another fresh setup lock exists", async () => {
    const { insertValues, selectWhere, deleteWhere } = createLockDb();
    insertValues.mockRejectedValueOnce(new Error("duplicate lock"));
    selectWhere.mockResolvedValueOnce([
      { id: "first-admin", lockedAt: 1_000 },
    ] satisfies FakeLockRecord[]);
    dateNow.mockReturnValueOnce(1_100).mockReturnValueOnce(1_200);

    const callback = jest.fn();
    const result = await new SetupLockRepository().withLock(callback);

    expect(result).toEqual({ acquired: false, value: null });
    expect(callback).not.toHaveBeenCalled();
    expect(deleteWhere).not.toHaveBeenCalled();
  });

  it("removes only the observed stale setup lock before acquiring its own lock", async () => {
    const staleLockedAt = 1_000;
    const acquiredLockedAt = 400_000;
    const { insertValues, selectWhere, deleteWhere } = createLockDb();
    insertValues
      .mockRejectedValueOnce(new Error("duplicate lock"))
      .mockResolvedValueOnce(undefined);
    selectWhere.mockResolvedValueOnce([
      { id: "first-admin", lockedAt: staleLockedAt },
    ] satisfies FakeLockRecord[]);
    dateNow
      .mockReturnValueOnce(300_000)
      .mockReturnValueOnce(350_001)
      .mockReturnValueOnce(acquiredLockedAt);

    const callback = jest.fn().mockResolvedValue("created");
    const result = await new SetupLockRepository().withLock(callback);

    expect(result).toEqual({ acquired: true, value: "created" });
    expect(callback).toHaveBeenCalledTimes(1);
    expect(deleteWhere).toHaveBeenCalledTimes(2);
    expect(conditionValues(deleteWhere.mock.calls[0][0])).toContain(
      staleLockedAt,
    );
    expect(conditionValues(deleteWhere.mock.calls[1][0])).toContain(
      acquiredLockedAt,
    );
  });

  it("removes only the observed stale admin mutation lock before acquiring its own lock", async () => {
    const staleLockedAt = 2_000;
    const acquiredLockedAt = 500_000;
    const { insertValues, selectWhere, deleteWhere } = createLockDb();
    insertValues
      .mockRejectedValueOnce(new Error("duplicate lock"))
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

    const repository = new SetupLockRepository();
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
      { acquired: true, value: "first" },
      { acquired: true, value: "second" },
    ]);
    expect(events).toEqual([
      "first started",
      "first finished",
      "second started",
    ]);
  });
});
