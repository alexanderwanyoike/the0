import { Test, TestingModule } from "@nestjs/testing";
import { ApiKeyRepository } from "../api-key.repository";
import { Ok, Failure } from "../../common/result";

jest.mock("../../database/connection", () => ({
  getDatabase: jest.fn().mockReturnValue({
    select: jest.fn().mockReturnValue({
      from: jest.fn().mockReturnValue({
        where: jest.fn().mockReturnValue([
          {
            id: "test-id",
            userId: "user-123",
            name: "Test Key",
            keyValue:
              "the0_abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234",
            isActive: true,
            createdAt: new Date(),
            updatedAt: new Date(),
          },
        ]),
      }),
    }),
    insert: jest.fn().mockReturnValue({
      values: jest.fn().mockReturnValue({
        returning: jest.fn().mockReturnValue([
          {
            id: "test-id",
            name: "Test Key",
            keyValue:
              "the0_abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234",
            isActive: true,
            createdAt: new Date(),
            updatedAt: new Date(),
          },
        ]),
      }),
    }),
    update: jest.fn().mockReturnValue({
      set: jest.fn().mockReturnValue({
        where: jest.fn().mockResolvedValue(undefined),
      }),
    }),
    delete: jest.fn().mockReturnValue({
      where: jest.fn().mockResolvedValue(undefined),
    }),
  }),
  getTables: jest.fn().mockReturnValue({
    apiKeys: {
      id: "id",
      userId: "userId",
      name: "name",
      keyValue: "keyValue",
      isActive: "isActive",
      lastUsedAt: "lastUsedAt",
      createdAt: "createdAt",
      updatedAt: "updatedAt",
    },
  }),
}));

jest.mock("crypto", () => ({
  randomBytes: jest.fn().mockReturnValue(Buffer.from("abcd1234".repeat(8))),
}));

describe("ApiKeyRepository", () => {
  let repository: ApiKeyRepository;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [ApiKeyRepository],
    }).compile();

    repository = module.get<ApiKeyRepository>(ApiKeyRepository);
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it("should be defined", () => {
    expect(repository).toBeDefined();
  });

  describe("createApiKey", () => {
    it("should create API key successfully", async () => {
      const result = await repository.createApiKey("user-123", "Test Key");

      expect(result.success).toBe(true);
      expect(result.data.name).toBe("Test Key");
    });
  });

  describe("findByKey", () => {
    it("should find API key by key value", async () => {
      const result = await repository.findByKey(
        "the0_abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234",
      );

      expect(result.success).toBe(true);
      expect(result.data.id).toBe("test-id");
    });
  });

  describe("updateLastUsed", () => {
    it("should update last used timestamp", async () => {
      const result = await repository.updateLastUsed("test-id");

      expect(result.success).toBe(true);
    });
  });

  describe("deleteApiKey", () => {
    it("should delete API key successfully", async () => {
      const result = await repository.deleteApiKey("user-123", "test-id");

      expect(result.success).toBe(true);
    });
  });
});
