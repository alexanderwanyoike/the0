import { Module } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { UploadController } from './upload.controller';
import { StorageService } from '../custom-bot/storage.service';

@Module({
  imports: [ConfigModule],
  controllers: [UploadController],
  providers: [StorageService],
})
export class UploadModule {}