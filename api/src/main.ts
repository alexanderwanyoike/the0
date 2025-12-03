import { ValidationPipe } from "@nestjs/common";
import { NestFactory } from "@nestjs/core";
import { Logger } from "nestjs-pino";
import { AppModule } from "./app.module";
import { runMigrations } from "./database/migrate";

async function bootstrap() {
  // Run database migrations before starting the application
  // Note: migrations run before NestJS app is created, so we use console here
  console.log("Running database migrations...");
  await runMigrations();
  console.log("Database migrations completed");

  const app = await NestFactory.create(AppModule, { bufferLogs: true });

  // Use Pino logger
  const logger = app.get(Logger);
  app.useLogger(logger);

  // Enable CORS for OSS version
  app.enableCors({
    origin: process.env.FRONTEND_URL || "http://localhost:3001",
    credentials: true,
  });

  // Global validation pipe
  app.useGlobalPipes(
    new ValidationPipe({
      whitelist: true,
      forbidNonWhitelisted: true,
      transform: true,
    }),
  );

  const port = process.env.PORT || 3000;
  await app.listen(port, "0.0.0.0");

  logger.log(`the0 API started on port ${port}`);
}

bootstrap().catch((error) => {
  console.error("Failed to start the0 API:", error);
  process.exit(1);
});
