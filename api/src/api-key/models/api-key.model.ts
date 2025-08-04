export interface ApiKey {
  id: string;
  userId: string;
  name: string;
  key: string;
  isActive: boolean;
  createdAt: Date;
  updatedAt: Date;
  lastUsedAt?: Date;
}
