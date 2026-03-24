import { Failure, Ok, errorMessage } from "../result";

describe("result", () => {
  describe("Ok", () => {
    it("should create a success result with data", () => {
      const result = Ok("test");
      expect(result.success).toBe(true);
      expect(result.data).toBe("test");
      expect(result.error).toBeNull();
    });
  });

  describe("Failure", () => {
    it("should create a failure result with error", () => {
      const result = Failure("something went wrong");
      expect(result.success).toBe(false);
      expect(result.error).toBe("something went wrong");
      expect(result.data).toBeNull();
    });
  });

  describe("errorMessage", () => {
    it("should extract message from Error instances", () => {
      const err = new Error("test error");
      expect(errorMessage(err)).toBe("test error");
    });

    it("should extract message from TypeError instances", () => {
      const err = new TypeError("type error");
      expect(errorMessage(err)).toBe("type error");
    });

    it("should convert string values to string", () => {
      expect(errorMessage("plain string error")).toBe("plain string error");
    });

    it("should convert number values to string", () => {
      expect(errorMessage(42)).toBe("42");
    });

    it("should convert null to string", () => {
      expect(errorMessage(null)).toBe("null");
    });

    it("should convert undefined to string", () => {
      expect(errorMessage(undefined)).toBe("undefined");
    });

    it("should convert objects to string", () => {
      expect(errorMessage({ code: "ERR" })).toBe("[object Object]");
    });
  });
});
