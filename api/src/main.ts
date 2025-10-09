import { ValidationPipe } from '@nestjs/common';
import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
import { runMigrations } from './database/migrate';


async function bootstrap() {
  // Run database migrations before starting the application
  console.log('🔄 Running database migrations...');
  await runMigrations();
  console.log('✅ Database migrations completed');
  
  const app = await NestFactory.create(AppModule);
  
  // Enable CORS for OSS version
  app.enableCors({
    origin: process.env.FRONTEND_URL || 'http://localhost:3001',
    credentials: true,
  });
  
  // Global validation pipe
  app.useGlobalPipes(new ValidationPipe({
    whitelist: true,
    forbidNonWhitelisted: true,
    transform: true,
  }));
  
  const port = process.env.PORT || 3000;
  await app.listen(port, '0.0.0.0');
  
  console.log(`🚀 The0 API is running on port ${port}`);
}

bootstrap().catch((error) => {
  console.error('Failed to start The0 API:', error);
  process.exit(1);
});
