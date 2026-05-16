import { getDatabase, getDatabaseConfig } from "./connection";
import { usersTable, usersTableSqlite } from "./schema/users";
import { hashPassword } from "@/common/password";
import { createId } from "@paralleldrive/cuid2";
import pino from "pino";

const logger = pino({ name: "seed" });
const DEFAULT_DEV_ADMIN_PASSWORD = "admin123";

function getSeedAdminPassword(): string {
  const password = process.env.THE0_SEED_ADMIN_PASSWORD;
  if (password) {
    return password;
  }

  const nodeEnv = process.env.NODE_ENV ?? "development";
  if (nodeEnv === "production" || nodeEnv === "staging") {
    throw new Error(
      "THE0_SEED_ADMIN_PASSWORD is required when seeding outside development",
    );
  }

  logger.warn(
    "Using default development seed admin password. Set THE0_SEED_ADMIN_PASSWORD to override.",
  );
  return DEFAULT_DEV_ADMIN_PASSWORD;
}

async function seedDatabase() {
  const config = getDatabaseConfig();
  const db = getDatabase();

  logger.info({ dbType: config.type }, "Seeding database");

  try {
    // Create default admin user
    const seedAdminPassword = getSeedAdminPassword();
    const adminPasswordHash = await hashPassword(seedAdminPassword);
    const adminUser = {
      id: createId(),
      username: "admin",
      email: "admin@the0.local",
      passwordHash: adminPasswordHash,
      firstName: "Admin",
      lastName: "User",
      role: "admin",
      isActive: true,
      isEmailVerified: true,
      metadata: { role: "admin", createdBy: "seed" },
    };

    if (config.type === "sqlite") {
      await db.insert(usersTableSqlite).values(adminUser).onConflictDoNothing();
    } else {
      await db.insert(usersTable).values(adminUser).onConflictDoNothing();
    }

    logger.info(
      "Admin user created: admin@the0.local / configured seed password",
    );
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
