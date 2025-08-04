import { ApiClient, ApiResponse } from '@/lib/api-client';
import { Result } from '@/lib/result';

// API User types to replace Firebase UserData
export interface ApiUser {
  id: string;
  email: string;
  displayName?: string | null;
  photoURL?: string | null;
  createdAt: string;     // ISO string instead of Timestamp
  updatedAt: string;     // ISO string instead of Timestamp  
  lastLogin?: string;    // ISO string instead of Timestamp
  isActive: boolean;
  isEmailVerified: boolean;
}

// User operation request types
export interface CreateUserRequest {
  email: string;
  displayName?: string;
  userId: string;
}

export interface UpdateUserRequest {
  displayName?: string;
  photoURL?: string;
}

export interface UserRepositoryError {
  error: string;
  status: number;
}

export class UserService {
  // Replace UserRepository.create()
  static async createUser(userData: CreateUserRequest): Promise<Result<ApiUser, UserRepositoryError>> {
    try {
      const response = await ApiClient.post<ApiUser>('/api/users', userData);
      if (!response.data) {
        return { 
          success: false, 
          error: { 
            error: response.error?.message || 'Failed to create user', 
            status: response.error?.statusCode || 400 
          } 
        };
      }
      return { success: true, data: response.data };
    } catch (error) {
      return { 
        success: false, 
        error: { error: 'Failed to create user', status: 500 } 
      };
    }
  }

  // Replace UserRepository.get() 
  static async getUser(userId: string): Promise<Result<ApiUser | null, UserRepositoryError>> {
    try {
      const response = await ApiClient.get<ApiUser>(`/api/users/${userId}`);
      if (!response.data) {
        if (response.error?.statusCode === 404) {
          return { success: true, data: null }; // User not found
        }
        return { 
          success: false, 
          error: { 
            error: response.error?.message || 'Failed to fetch user', 
            status: response.error?.statusCode || 400 
          } 
        };
      }
      return { success: true, data: response.data };
    } catch (error) {
      return { 
        success: false, 
        error: { error: 'Failed to fetch user', status: 500 } 
      };
    }
  }

  // Replace UserRepository.update()
  static async updateUser(userId: string, data: UpdateUserRequest): Promise<Result<ApiUser, UserRepositoryError>> {
    try {
      const response = await ApiClient.put<ApiUser>(`/api/users/${userId}`, data);
      if (!response.data) {
        return { 
          success: false, 
          error: { 
            error: response.error?.message || 'Failed to update user', 
            status: response.error?.statusCode || 400 
          } 
        };
      }
      return { success: true, data: response.data };
    } catch (error) {
      return { 
        success: false, 
        error: { error: 'Failed to update user', status: 500 } 
      };
    }
  }

  // Replace UserRepository.updateLastLogin()
  static async updateLastLogin(userId: string): Promise<Result<null, UserRepositoryError>> {
    try {
      const lastLogin = new Date().toISOString(); // Replace serverTimestamp()
      const response = await ApiClient.put(`/api/users/${userId}`, { lastLogin });
      if (response.error) {
        return { 
          success: false, 
          error: { 
            error: response.error.message || 'Failed to update last login', 
            status: response.error.statusCode || 400 
          } 
        };
      }
      return { success: true, data: null };
    } catch (error) {
      return { 
        success: false, 
        error: { error: 'Failed to update last login', status: 500 } 
      };
    }
  }

  // Replace UserRepository.deleteUser()
  static async deleteUser(userId: string): Promise<Result<null, UserRepositoryError>> {
    try {
      const response = await ApiClient.delete(`/api/users/${userId}`);
      if (response.error) {
        return { 
          success: false, 
          error: { 
            error: response.error.message || 'Failed to delete user', 
            status: response.error.statusCode || 400 
          } 
        };
      }
      return { success: true, data: null };
    } catch (error) {
      return { 
        success: false, 
        error: { error: 'Failed to delete user', status: 500 } 
      };
    }
  }
}