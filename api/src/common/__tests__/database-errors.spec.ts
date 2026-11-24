import { DrizzleQueryError } from "drizzle-orm";
import {
  isForeignKeyViolation,
  isUniqueConstraintError,
} from "../database-errors";

const driverError = (code: string) =>
  Object.assign(new Error(`database error ${code}`), { code });

const queryError = (code: string) =>
  new DrizzleQueryError('delete from "custom_bots"', [], driverError(code));

describe("isUniqueConstraintError", () => {
  it("recognises a uniqueness violation wrapped by Drizzle", () => {
    expect(isUniqueConstraintError(queryError("23505"))).toBe(true);
  });

  it("keeps direct driver errors supported", () => {
    expect(isUniqueConstraintError(driverError("23505"))).toBe(true);
    expect(
      isUniqueConstraintError(driverError("SQLITE_CONSTRAINT_UNIQUE")),
    ).toBe(true);
  });

  it("does not treat other database failures as unique violations", () => {
    expect(isUniqueConstraintError(queryError("23503"))).toBe(false);
    expect(isUniqueConstraintError(undefined)).toBe(false);
  });

  it.each(["constraint", "constraint_name"])(
    "matches the named constraint reported through %s",
    (property) => {
      const wrapped = new DrizzleQueryError(
        "insert into bots",
        [],
        Object.assign(driverError("23505"), {
          [property]: "bots_user_id_name_unique",
        }),
      );
      expect(isUniqueConstraintError(wrapped, "bots_user_id_name_unique")).toBe(
        true,
      );
      expect(isUniqueConstraintError(wrapped, "bots_pkey")).toBe(false);
      expect(
        isUniqueConstraintError(
          queryError("23505"),
          "bots_user_id_name_unique",
        ),
      ).toBe(false);
    },
  );
});

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
