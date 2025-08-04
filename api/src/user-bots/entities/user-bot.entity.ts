import { BaseEntity } from '@/common/entites/base.entity';

export interface UserBot extends BaseEntity {
  userId: string;
  customBotName: string;
  acquiredAt: Date;
}
