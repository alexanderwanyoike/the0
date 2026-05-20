import * as bcrypt from "bcrypt";

const DEFAULT_BCRYPT_SALT_ROUNDS = 12;
const MIN_BCRYPT_SALT_ROUNDS = 12;

export function getBcryptSaltRounds(): number {
  const rawRounds = process.env.BCRYPT_SALT_ROUNDS;
  if (!rawRounds) {
    return DEFAULT_BCRYPT_SALT_ROUNDS;
  }

  const rounds = Number(rawRounds);
  if (
    !Number.isInteger(rounds) ||
    rounds < MIN_BCRYPT_SALT_ROUNDS ||
    rounds > 31
  ) {
    throw new Error(
      `BCRYPT_SALT_ROUNDS must be an integer between ${MIN_BCRYPT_SALT_ROUNDS} and 31`,
    );
  }

  return rounds;
}

export function hashPassword(password: string): Promise<string> {
  return bcrypt.hash(password, getBcryptSaltRounds());
}
