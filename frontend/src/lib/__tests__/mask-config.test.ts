import { maskSensitiveConfig } from "../mask-config";

describe("maskSensitiveConfig", () => {
  it("removes credential-like keys at any depth and keeps the rest", () => {
    const config = {
      name: "bot",
      symbol: "BTCUSD",
      api_key: "k",
      apiSecret: "s",
      password: "p",
      exchange: { name: "x", exchangeSecretValue: "s", accessToken: "t" },
      legs: [{ side: "buy", passphrase: "pp" }],
      monkey: "matches key$",
      spinner: "matches pin",
    };

    expect(maskSensitiveConfig(config)).toEqual({
      name: "bot",
      symbol: "BTCUSD",
      exchange: { name: "x" },
      legs: [{ side: "buy" }],
    });
  });

  it("does not mutate the input and drops values JSON cannot carry", () => {
    const config = { nested: { token: "t", keep: 1 }, fn: () => 1 };
    const masked = maskSensitiveConfig(config);

    expect(config.nested.token).toBe("t");
    expect(masked).toEqual({ nested: { keep: 1 } });
  });

  it("preserves null and primitive values", () => {
    expect(maskSensitiveConfig({ a: null, b: 0, c: false })).toEqual({
      a: null,
      b: 0,
      c: false,
    });
  });
});
