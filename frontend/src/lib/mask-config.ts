const SENSITIVE_KEY_PATTERNS = [
  /api[_-]?key/i,
  /secret[_-]?key/i,
  /api[_-]?secret/i,
  /password/i,
  /token/i,
  /key$/i,
  /private[_-]?key/i,
  /access[_-]?token/i,
  /refresh[_-]?token/i,
  /auth[_-]?token/i,
  /bearer[_-]?token/i,
  /exchange.*key/i,
  /exchange.*secret/i,
  /credential/i,
  /passphrase/i,
  /pin/i,
];

const isSensitiveKey = (key: string) =>
  SENSITIVE_KEY_PATTERNS.some((p) => p.test(key));

function withoutSensitiveKeys(obj: any): any {
  if (!obj || typeof obj !== "object") return obj;
  const filtered: any = Array.isArray(obj) ? [] : {};
  Object.keys(obj).forEach((key) => {
    if (isSensitiveKey(key)) return;
    filtered[key] =
      typeof obj[key] === "object" && obj[key] !== null
        ? withoutSensitiveKeys(obj[key])
        : obj[key];
  });
  return filtered;
}

/** Deep copy of a bot config with credential-like keys removed at any depth,
 *  safe to display or copy to the clipboard. */
export function maskSensitiveConfig(config: Record<string, any>): any {
  return withoutSensitiveKeys(JSON.parse(JSON.stringify(config)));
}
