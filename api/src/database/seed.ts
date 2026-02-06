import { getDatabase, getDatabaseConfig } from "./connection";
import { usersTable, usersTableSqlite } from "./schema/users";
import { hash } from "bcrypt";
import { createId } from "@paralleldrive/cuid2";
import pino from "pino";

const logger = pino({ name: "seed" });

async function seedDatabase() {
  const config = getDatabaseConfig();
  const db = getDatabase();

  logger.info({ dbType: config.type }, "Seeding database");

  try {
    // Create default admin user
    const adminPasswordHash = await hash("admin123", 10);
    const adminUser = {
      id: createId(),
      username: "admin",
      email: "admin@the0.local",
      passwordHash: adminPasswordHash,
      firstName: "Admin",
      lastName: "User",
      isActive: true,
      isEmailVerified: true,
      metadata: { role: "admin", createdBy: "seed" },
    };

    if (config.type === "sqlite") {
      await db.insert(usersTableSqlite).values(adminUser).onConflictDoNothing();
    } else {
      await db.insert(usersTable).values(adminUser).onConflictDoNothing();
    }

    logger.info("Admin user created: admin@the0.local / admin123");
    logger.info("Database seeding completed successfully");
  } catch (error) {
    logger.error({ err: error }, "Database seeding failed");
    process.exit(1);
  }
}

// Run seeding if this file is executed directly
if (require.main === module) {
  seedDatabase()
    .then(() => {
      logger.info("Database seeding completed");
      process.exit(0);
    })
    .catch((error) => {
      logger.error({ err: error }, "Seeding script failed");
      process.exit(1);
    });
}

export { seedDatabase };
