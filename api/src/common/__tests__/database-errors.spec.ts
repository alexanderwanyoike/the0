import { DrizzleQueryError } from "drizzle-orm";
import { isForeignKeyViolation } from "../database-errors";

const driverError = (code: string) =>
  Object.assign(new Error(`database error ${code}`), { code });

const queryError = (code: string) =>
  new DrizzleQueryError('delete from "custom_bots"', [], driverError(code));

describe("isForeignKeyViolation", () => {
  it("recognises a delete a foreign key refused on Postgres before 18", () => {
    expect(isForeignKeyViolation(queryError("23503"))).toBe(true);
  });

  it("recognises a delete an ON DELETE RESTRICT foreign key refused on Postgres 18", () => {
    expect(isForeignKeyViolation(queryError("23001"))).toBe(true);
  });

  it("recognises the violation when the driver error is not wrapped", () => {
    expect(isForeignKeyViolation(driverError("23503"))).toBe(true);
  });

  it("does not treat other failures as a foreign key violation", () => {
    expect(isForeignKeyViolation(queryError("23505"))).toBe(false);
    expect(isForeignKeyViolation(driverError("ECONNREFUSED"))).toBe(false);
    expect(isForeignKeyViolation(new Error("boom"))).toBe(false);
    expect(isForeignKeyViolation(undefined)).toBe(false);
  });
});
