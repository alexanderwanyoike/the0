/**
 * the0 State Module
 * =================
 *
 * Provides persistent state management for bots across executions.
 * State is automatically synced to MinIO storage between bot runs.
 *
 * @example
 * ```typescript
 * import { state } from '@the0/sdk';
 *
 * // Store state
 * state.set('portfolio', { AAPL: 100, GOOGL: 50 });
 *
 * // Retrieve state
 * const portfolio = state.get('portfolio', {});
 *
 * // List all keys
 * const keys = state.list();
 *
 * // Delete a key
 * state.delete('portfolio');
 *
 * // Clear all state
 * state.clear();
 * ```
 */

import * as fs from 'fs';
import * as path from 'path';

/**
 * Get the path to the state directory.
 */
function getStateDir(): string {
  return process.env.STATE_DIR || '/state/.the0-state';
}

/**
 * Get the file path for a state key.
 */
function getKeyPath(key: string): string {
  return path.join(getStateDir(), `${key}.json`);
}

/**
 * Validate that a key is safe to use as a filename.
 */
function validateKey(key: string): void {
  if (!key) {
    throw new Error('State key cannot be empty');
  }
  // Prevent directory traversal
  if (key.includes('/') || key.includes('\\') || key.includes('..')) {
    throw new Error("State key cannot contain path separators or '..'");
  }
}

/**
 * Get a value from persistent state.
 *
 * @param key - The state key (alphanumeric, hyphens, underscores)
 * @param defaultValue - Default value if key doesn't exist
 * @returns The stored value, or default if not found
 *
 * @example
 * ```typescript
 * const portfolio = state.get('portfolio', {});
 * const tradeCount = state.get<number>('trade_count', 0);
 * ```
 */
export function get<T>(key: string, defaultValue?: T): T | undefined {
  validateKey(key);
  const filepath = getKeyPath(key);
  try {
    const content = fs.readFileSync(filepath, 'utf-8');
    return JSON.parse(content) as T;
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === 'ENOENT') {
      return defaultValue;
    }
    // JSON parse error or other error
    return defaultValue;
  }
}

/**
 * Set a value in persistent state.
 *
 * The value must be JSON serializable.
 *
 * @param key - The state key (alphanumeric, hyphens, underscores)
 * @param value - The value to store (must be JSON serializable)
 *
 * @example
 * ```typescript
 * state.set('portfolio', { AAPL: 100, GOOGL: 50 });
 * state.set('trade_count', 42);
 * state.set('last_prices', [45000.5, 45100.0, 45050.25]);
 * ```
 */
export function set<T>(key: string, value: T): void {
  validateKey(key);
  const stateDir = getStateDir();
  fs.mkdirSync(stateDir, { recursive: true });
  const filepath = getKeyPath(key);
  fs.writeFileSync(filepath, JSON.stringify(value));
}

/**
 * Delete a key from persistent state.
 *
 * @param key - The state key to delete
 * @returns True if the key existed and was deleted, false otherwise
 *
 * @example
 * ```typescript
 * if (state.delete('old_data')) {
 *   console.log('Cleaned up old data');
 * }
 * ```
 */
export function remove(key: string): boolean {
  validateKey(key);
  const filepath = getKeyPath(key);
  try {
    fs.unlinkSync(filepath);
    return true;
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === 'ENOENT') {
      return false;
    }
    throw err;
  }
}

// Alias for delete (since 'delete' is a reserved word in some contexts)
export { remove as delete };

/**
 * List all keys in persistent state.
 *
 * @returns List of state keys
 *
 * @example
 * ```typescript
 * const keys = state.list();
 * console.log(`State contains ${keys.length} keys: ${keys}`);
 * ```
 */
export function list(): string[] {
  const stateDir = getStateDir();
  try {
    const files = fs.readdirSync(stateDir);
    return files
      .filter((f) => f.endsWith('.json'))
      .map((f) => f.slice(0, -5));
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === 'ENOENT') {
      return [];
    }
    throw err;
  }
}

/**
 * Clear all state.
 *
 * Removes all stored state keys.
 *
 * @example
 * ```typescript
 * state.clear();
 * console.log('All state cleared');
 * ```
 */
export function clear(): void {
  const stateDir = getStateDir();
  try {
    const files = fs.readdirSync(stateDir);
    for (const filename of files) {
      if (filename.endsWith('.json')) {
        fs.unlinkSync(path.join(stateDir, filename));
      }
    }
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === 'ENOENT') {
      return;
    }
    throw err;
  }
}

/**
 * Check if a key exists in state.
 *
 * @param key - The state key to check
 * @returns True if the key exists, false otherwise
 *
 * @example
 * ```typescript
 * if (state.exists('portfolio')) {
 *   const portfolio = state.get('portfolio');
 * }
 * ```
 */
export function exists(key: string): boolean {
  validateKey(key);
  const filepath = getKeyPath(key);
  return fs.existsSync(filepath);
}
