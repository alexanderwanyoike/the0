// src/api-keys/services/api-key.service.ts
import { Injectable } from "@nestjs/common";
import { ApiKeyRepository } from "./api-key.repository";
import { Result, Ok, Failure } from "../common/result";
import { ApiKey } from "./models/api-key.model";
import { CreateApiKeyDto } from "./dto/create-api-key.dto";
import { ApiKeyCreatedResponseDto } from "./dto/api-key-created-response.dto";
import { ApiKeyResponseDto } from "./dto/api-key-response.dto";

@Injectable()
export class ApiKeyService {
  constructor(private readonly apiKeyRepository: ApiKeyRepository) {}

  /**
   * Create a new API key
   */
  async createApiKey(
    userId: string,
    createApiKeyDto: CreateApiKeyDto,
  ): Promise<Result<ApiKeyCreatedResponseDto, string>> {
    // Check if user already has an API key with this name
    const existingKeys = await this.apiKeyRepository.findAll(userId);
    if (!existingKeys.success) {
      return Failure(existingKeys.error);
    }

    const nameExists = existingKeys.data.some(
      (key) => key.name === createApiKeyDto.name,
    );
    if (nameExists) {
      return Failure("An API key with this name already exists");
    }

    // Create the API key
    const result = await this.apiKeyRepository.createApiKey(
      userId,
      createApiKeyDto.name,
    );
    if (!result.success) {
      return Failure(result.error);
    }

    // Return the created API key with full key
    const responseDto: ApiKeyCreatedResponseDto = {
      id: result.data.id,
      userId: result.data.userId,
      name: result.data.name,
      key: result.data.key,
      isActive: result.data.isActive,
      createdAt: result.data.createdAt,
      updatedAt: result.data.updatedAt,
      lastUsedAt: result.data.lastUsedAt,
    };

    return Ok(responseDto);
  }

  /**
   * Get all API keys for a user (with full keys)
   */
  async getUserApiKeys(
    userId: string,
  ): Promise<Result<ApiKeyResponseDto[], string>> {
    const result = await this.apiKeyRepository.findAll(userId);
    if (!result.success) {
      return Failure(result.error);
    }

    // Transform to response DTOs (including full key)
    const responseDtos: ApiKeyResponseDto[] = result.data.map((apiKey) => ({
      id: apiKey.id,
      userId: apiKey.userId,
      name: apiKey.name,
      key: apiKey.key,
      isActive: apiKey.isActive,
      createdAt: apiKey.createdAt,
      updatedAt: apiKey.updatedAt,
      lastUsedAt: apiKey.lastUsedAt,
    }));

    return Ok(responseDtos);
  }

  /**
   * Get a specific API key by ID (with full key)
   */
  async getApiKeyById(
    userId: string,
    keyId: string,
  ): Promise<Result<ApiKeyResponseDto, string>> {
    const result = await this.apiKeyRepository.findOne(userId, keyId);
    if (!result.success) {
      return Failure(result.error);
    }

    // Transform to response DTO (including full key)
    const responseDto: ApiKeyResponseDto = {
      id: result.data.id,
      userId: result.data.userId,
      name: result.data.name,
      key: result.data.key,
      isActive: result.data.isActive,
      createdAt: result.data.createdAt,
      updatedAt: result.data.updatedAt,
      lastUsedAt: result.data.lastUsedAt,
    };

    return Ok(responseDto);
  }

  /**
   * Delete (deactivate) an API key
   */
  async deleteApiKey(
    userId: string,
    keyId: string,
  ): Promise<Result<void, string>> {
    const result = await this.apiKeyRepository.deleteApiKey(userId, keyId);
    if (!result.success) {
      return Failure(result.error);
    }

    return Ok(null);
  }

  /**
   * Validate an API key (for authentication middleware)
   */
  async validateApiKey(key: string): Promise<Result<ApiKey, string>> {
    const result = await this.apiKeyRepository.findByKey(key);
    if (!result.success) {
      return Failure(result.error);
    }

    // Update last used timestamp
    await this.apiKeyRepository.updateLastUsed(result.data.id);

    return Ok(result.data);
  }

  /**
   * Get API key usage statistics
   */
  async getApiKeyStats(
    userId: string,
  ): Promise<Result<{ total: number; active: number }, string>> {
    const result = await this.apiKeyRepository.findAll(userId);
    if (!result.success) {
      return Failure(result.error);
    }

    const stats = {
      total: result.data.length,
      active: result.data.filter((key) => key.isActive).length,
    };

    return Ok(stats);
  }
}
