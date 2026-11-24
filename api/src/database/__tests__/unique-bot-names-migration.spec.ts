import { PGlite } from "@electric-sql/pglite";
import { readFileSync } from "fs";
import { join } from "path";

const migration = () =>
  readFileSync(
    join(__dirname, "../migrations/0009_unique_bot_names.sql"),
    "utf8",
  );

describe("the unique bot names migration", () => {
  let db: PGlite;

  beforeEach(async () => {
    db = new PGlite();
    await db.exec(
      "CREATE TABLE bots (id text PRIMARY KEY, user_id text NOT NULL, name text NOT NULL)",
    );
  });

  afterEach(async () => {
    await db.close();
  });

  it("retains existing distinct names and rejects a new duplicate", async () => {
    await db.exec(
      "INSERT INTO bots VALUES ('1', 'owner', 'SMA'), ('2', 'other', 'SMA'), ('3', 'owner', 'sma')",
    );
    const before = await db.query("SELECT * FROM bots ORDER BY id");

    await db.exec(migration());

    expect((await db.query("SELECT * FROM bots ORDER BY id")).rows).toEqual(
      before.rows,
    );
    await expect(
      db.exec("INSERT INTO bots VALUES ('4', 'owner', 'SMA')"),
    ).rejects.toMatchObject({ code: "23505" });
  });

  it("refuses an upgrade with duplicates without renaming or deleting them", async () => {
    await db.exec(
      "INSERT INTO bots VALUES ('1', 'owner', 'SMA'), ('2', 'owner', 'SMA')",
    );
    const before = await db.query("SELECT * FROM bots ORDER BY id");

    await expect(db.exec(migration())).rejects.toMatchObject({ code: "23505" });

    expect((await db.query("SELECT * FROM bots ORDER BY id")).rows).toEqual(
      before.rows,
    );
    await db.exec("UPDATE bots SET name = 'Other' WHERE id = '2'");
    await db.exec(migration());
    await expect(
      db.exec("INSERT INTO bots VALUES ('3', 'owner', 'SMA')"),
    ).rejects.toMatchObject({ code: "23505" });
  });
});
