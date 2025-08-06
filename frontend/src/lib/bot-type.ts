export const sanitizeBotType = (botType: string): string =>
  botType.replace(/\//g, "_");

export const splitBotType = (
  botType: string,
): {
  vendor: string;
  type: string;
  name: string;
} => {
  const [vendor, type, name] = botType.split("/");
  if (!vendor || !type || !name) {
    throw new Error(
      "Invalid bot type format. Expected format: vendor/type/name",
    );
  }
  return { vendor, type, name };
};
