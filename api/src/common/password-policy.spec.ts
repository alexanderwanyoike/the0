import { MIN_PASSWORD_LENGTH, validatePasswordPolicy } from "./password-policy";

describe("password policy", () => {
  it("rejects passwords shorter than the minimum length", () => {
    expect(validatePasswordPolicy("1234567")).toBe(
      `Password must be at least ${MIN_PASSWORD_LENGTH} characters long`,
    );
  });

  it("allows passwords at the minimum length", () => {
    expect(validatePasswordPolicy("12345678")).toBeNull();
  });
});
