import {
  validatePlatform,
  getPlatformDisplayName,
  getAllPlatforms,
  suggestPlatformFromHeaders,
} from "../platform-detection";
import type { PlatformId } from "@/types/install";

describe("Platform Detection - Simple Tests", () => {
  beforeEach(() => {
    process.env.NEXT_PUBLIC_CLI_INSTALL_BASE_URL = "http://localhost:3000";
  });

  describe("validatePlatform", () => {
    it("should validate supported platforms", () => {
      expect(validatePlatform("darwin-amd64")).toBe(true);
      expect(validatePlatform("darwin-arm64")).toBe(true);
      expect(validatePlatform("linux-amd64")).toBe(true);
      expect(validatePlatform("linux-arm64")).toBe(true);
      expect(validatePlatform("windows-amd64")).toBe(true);
    });

    it("should reject unsupported platforms", () => {
      expect(validatePlatform("unknown-platform")).toBe(false);
      expect(validatePlatform("windows-arm64")).toBe(false);
      expect(validatePlatform("")).toBe(false);
    });
  });

  describe("getPlatformDisplayName", () => {
    it("should return correct display names", () => {
      expect(getPlatformDisplayName("darwin-amd64")).toBe("macOS (Intel)");
      expect(getPlatformDisplayName("darwin-arm64")).toBe(
        "macOS (Apple Silicon)",
      );
      expect(getPlatformDisplayName("linux-amd64")).toBe("Linux (x64)");
      expect(getPlatformDisplayName("linux-arm64")).toBe("Linux (ARM64)");
      expect(getPlatformDisplayName("windows-amd64")).toBe("Windows");
    });

    it("should return platform ID for unknown platforms", () => {
      expect(getPlatformDisplayName("unknown" as PlatformId)).toBe("unknown");
    });
  });

  describe("getAllPlatforms", () => {
    it("should return all supported platforms", () => {
      const platforms = getAllPlatforms();
      expect(platforms).toHaveLength(5);
      expect(platforms.map((p) => p.id)).toEqual([
        "darwin-amd64",
        "darwin-arm64",
        "linux-amd64",
        "linux-arm64",
        "windows-amd64",
      ]);
    });

    it("should have correct platform properties", () => {
      const platforms = getAllPlatforms();
      const macOS = platforms.find((p) => p.id === "darwin-amd64");

      expect(macOS).toEqual({
        id: "darwin-amd64",
        os: "macOS",
        arch: "x64",
        displayName: "macOS (Intel)",
        command:
          "curl -fsSL http://localhost:3000/api/install/darwin-amd64 | bash",
        scriptUrl: "/api/install/darwin-amd64",
        fileExtension: "sh",
        shellCommand: "bash",
      });
    });
  });

  describe("suggestPlatformFromHeaders", () => {
    it("should detect macOS from user agent", () => {
      const userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)";
      const result = suggestPlatformFromHeaders(userAgent);

      expect(result).toBeTruthy();
      expect(result?.id).toBe("darwin-amd64");
      expect(result?.os).toBe("macOS");
    });

    it("should detect Windows from user agent", () => {
      const userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)";
      const result = suggestPlatformFromHeaders(userAgent);

      expect(result).toBeTruthy();
      expect(result?.id).toBe("windows-amd64");
      expect(result?.os).toBe("Windows");
    });

    it("should detect Linux from user agent", () => {
      const userAgent = "Mozilla/5.0 (X11; Linux x86_64)";
      const result = suggestPlatformFromHeaders(userAgent);

      expect(result).toBeTruthy();
      expect(result?.id).toBe("linux-amd64");
      expect(result?.os).toBe("Linux");
    });

    it("should return null for unknown user agent", () => {
      const result = suggestPlatformFromHeaders("Unknown browser");
      expect(result).toBeNull();
    });

    it("should return null for undefined user agent", () => {
      const result = suggestPlatformFromHeaders(undefined);
      expect(result).toBeNull();
    });
  });
});
