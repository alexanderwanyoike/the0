import { Test, TestingModule } from "@nestjs/testing";
import { HttpException, HttpStatus } from "@nestjs/common";
import { ApiKeyController } from "@/api-key/api-key.controller";
import { ApiKeyService } from "@/api-key/api-key.service";
import { CreateApiKeyDto } from "@/api-key/dto/create-api-key.dto";
import { Ok, Failure } from "@/common/result";

describe("ApiKeyController", () => {
  let controller: ApiKeyController;
  let service: jest.Mocked<ApiKeyService>;

  const mockRequest = {
    user: { uid: "user-123" },
    headers: {},
  };

  const mockApiKeyResponse = {
    id: "test-id",
    userId: "user-123",
    name: "Test API Key",
    key: "the0_abcdef123456789abcdef123456789abcdef123456789abcdef123456789",
    isActive: true,
    createdAt: new Date(),
    updatedAt: new Date(),
  };

  beforeEach(async () => {
    const mockService = {
      createApiKey: jest.fn(),
      getUserApiKeys: jest.fn(),
      getApiKeyById: jest.fn(),
      deleteApiKey: jest.fn(),
      getApiKeyStats: jest.fn(),
    };

    const module: TestingModule = await Test.createTestingModule({
      controllers: [ApiKeyController],
      providers: [
        {
          provide: ApiKeyService,
          useValue: mockService,
        },
      ],
    }).compile();

    controller = module.get<ApiKeyController>(ApiKeyController);
    service = module.get(ApiKeyService);
  });

  describe("createApiKey", () => {
    it("should create API key successfully", async () => {
      const createDto: CreateApiKeyDto = { name: "Test API Key" };
      const createdResponse = {
        ...mockApiKeyResponse,
        key: "the0_abcdef123456789",
      };

      service.createApiKey.mockResolvedValue(Ok(createdResponse));

      const result = await controller.createApiKey(mockRequest, createDto);

      expect(result).toEqual(createdResponse);
      expect(service.createApiKey).toHaveBeenCalledWith("user-123", createDto);
    });

    it("should throw BadRequestException on service failure", async () => {
      const createDto: CreateApiKeyDto = { name: "Test API Key" };
      service.createApiKey.mockResolvedValue(
        Failure("API key name already exists"),
      );

      await expect(
        controller.createApiKey(mockRequest, createDto),
      ).rejects.toThrow(
        new HttpException(
          {
            statusCode: HttpStatus.BAD_REQUEST,
            message: "API key name already exists",
            error: "Bad Request",
          },
          HttpStatus.BAD_REQUEST,
        ),
      );
    });
  });

  describe("getApiKeys", () => {
    it("should return user API keys", async () => {
      service.getUserApiKeys.mockResolvedValue(Ok([mockApiKeyResponse]));

      const result = await controller.getApiKeys(mockRequest);

      expect(result).toEqual([mockApiKeyResponse]);
      expect(service.getUserApiKeys).toHaveBeenCalledWith("user-123");
    });

    it("should throw InternalServerErrorException on service failure", async () => {
      service.getUserApiKeys.mockResolvedValue(Failure("Database error"));

      await expect(controller.getApiKeys(mockRequest)).rejects.toThrow(
        new HttpException(
          {
            statusCode: HttpStatus.INTERNAL_SERVER_ERROR,
            message: "Database error",
            error: "Internal Server Error",
          },
          HttpStatus.INTERNAL_SERVER_ERROR,
        ),
      );
    });
  });

  describe("getApiKeyById", () => {
    it("should return specific API key", async () => {
      service.getApiKeyById.mockResolvedValue(Ok(mockApiKeyResponse));

      const result = await controller.getApiKeyById(mockRequest, "test-id");

      expect(result).toEqual(mockApiKeyResponse);
      expect(service.getApiKeyById).toHaveBeenCalledWith("user-123", "test-id");
    });

    it("should throw NotFoundException when API key not found", async () => {
      service.getApiKeyById.mockResolvedValue(Failure("Not found"));

      await expect(
        controller.getApiKeyById(mockRequest, "test-id"),
      ).rejects.toThrow(
        new HttpException(
          {
            statusCode: HttpStatus.NOT_FOUND,
            message: "Not found",
            error: "Not Found",
          },
          HttpStatus.NOT_FOUND,
        ),
      );
    });
  });

  describe("deleteApiKey", () => {
    it("should delete API key successfully", async () => {
      service.deleteApiKey.mockResolvedValue(Ok(null));

      const result = await controller.deleteApiKey(mockRequest, "test-id");

      expect(result).toEqual({ message: "API key deleted successfully" });
      expect(service.deleteApiKey).toHaveBeenCalledWith("user-123", "test-id");
    });

    it("should throw NotFoundException when API key not found", async () => {
      service.deleteApiKey.mockResolvedValue(Failure("API key not found"));

      await expect(
        controller.deleteApiKey(mockRequest, "test-id"),
      ).rejects.toThrow(
        new HttpException(
          {
            statusCode: HttpStatus.NOT_FOUND,
            message: "API key not found",
            error: "Not Found",
          },
          HttpStatus.NOT_FOUND,
        ),
      );
    });
  });

  describe("getApiKeyStats", () => {
    it("should return API key statistics", async () => {
      const stats = { total: 2, active: 2 };
      service.getApiKeyStats.mockResolvedValue(Ok(stats));

      const result = await controller.getApiKeyStats(mockRequest);

      expect(result).toEqual(stats);
      expect(service.getApiKeyStats).toHaveBeenCalledWith("user-123");
    });

    it("should throw InternalServerErrorException on service failure", async () => {
      service.getApiKeyStats.mockResolvedValue(Failure("Database error"));

      await expect(controller.getApiKeyStats(mockRequest)).rejects.toThrow(
        new HttpException(
          {
            statusCode: HttpStatus.INTERNAL_SERVER_ERROR,
            message: "Database error",
            error: "Internal Server Error",
          },
          HttpStatus.INTERNAL_SERVER_ERROR,
        ),
      );
    });
  });
});
