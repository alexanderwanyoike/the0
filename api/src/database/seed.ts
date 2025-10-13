import { getDatabase, getDatabaseConfig } from "./connection";
import { usersTable, usersTableSqlite } from "./schema/users";
import { hash } from "bcrypt";
import { createId } from "@paralleldrive/cuid2";

async function seedDatabase() {
  const config = getDatabaseConfig();
  const db = getDatabase();

  console.log(`Seeding ${config.type} database...`);

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

    console.log("✅ Admin user created: admin@the0.local / admin123");
    console.log("✅ Database seeding completed successfully");
  } catch (error) {
    console.error("❌ Database seeding failed:", error);
    process.exit(1);
  }
}

// Run seeding if this file is executed directly
if (require.main === module) {
  seedDatabase()
    .then(() => {
      console.log("🎉 Database seeding completed");
      process.exit(0);
    })
    .catch((error) => {
      console.error("❌ Seeding script failed:", error);
      process.exit(1);
    });
}

export { seedDatabase };
