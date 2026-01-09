/**
 * Tests for the0 Node.js SDK state module
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import * as fs from 'fs';
import * as path from 'path';

import * as state from './state';

describe('state', () => {
  const originalEnv = process.env;
  let tempDir: string;

  beforeEach(() => {
    tempDir = fs.mkdtempSync('/tmp/the0-state-test-');
    process.env = { ...originalEnv, STATE_DIR: tempDir };
  });

  afterEach(() => {
    process.env = originalEnv;
    fs.rmSync(tempDir, { recursive: true, force: true });
  });

  describe('set and get', () => {
    it('should store and retrieve a dictionary', () => {
      const portfolio = { AAPL: 100, GOOGL: 50 };
      state.set('portfolio', portfolio);
      const retrieved = state.get('portfolio');
      expect(retrieved).toEqual(portfolio);
    });

    it('should store and retrieve an array', () => {
      const prices = [45000.5, 45100.0, 45050.25];
      state.set('prices', prices);
      const retrieved = state.get('prices');
      expect(retrieved).toEqual(prices);
    });

    it('should store and retrieve a number', () => {
      state.set('count', 42);
      expect(state.get('count')).toBe(42);
    });

    it('should store and retrieve a string', () => {
      state.set('symbol', 'BTC/USD');
      expect(state.get('symbol')).toBe('BTC/USD');
    });

    it('should store and retrieve a boolean', () => {
      state.set('active', true);
      expect(state.get('active')).toBe(true);
    });

    it('should store and retrieve null', () => {
      state.set('nothing', null);
      expect(state.get('nothing')).toBeNull();
    });
  });

  describe('get with defaults', () => {
    it('should return default for non-existent key', () => {
      const result = state.get('nonexistent', { default: true });
      expect(result).toEqual({ default: true });
    });

    it('should return undefined for non-existent key without default', () => {
      const result = state.get('nonexistent');
      expect(result).toBeUndefined();
    });

    it('should not use default when key exists', () => {
      state.set('key', 'actual');
      const result = state.get('key', 'default');
      expect(result).toBe('actual');
    });
  });

  describe('exists', () => {
    it('should return true for existing key', () => {
      state.set('exists_test', { value: 1 });
      expect(state.exists('exists_test')).toBe(true);
    });

    it('should return false for non-existent key', () => {
      expect(state.exists('nonexistent')).toBe(false);
    });
  });

  describe('delete', () => {
    it('should delete an existing key', () => {
      state.set('to_delete', 'value');
      expect(state.exists('to_delete')).toBe(true);
      const result = state.remove('to_delete');
      expect(result).toBe(true);
      expect(state.exists('to_delete')).toBe(false);
    });

    it('should return false for non-existent key', () => {
      const result = state.remove('nonexistent');
      expect(result).toBe(false);
    });
  });

  describe('list', () => {
    it('should list all keys', () => {
      state.set('key1', 'value1');
      state.set('key2', 'value2');
      state.set('key3', 'value3');
      const keys = state.list();
      expect(keys.sort()).toEqual(['key1', 'key2', 'key3']);
    });

    it('should return empty array when state is empty', () => {
      const keys = state.list();
      expect(keys).toEqual([]);
    });
  });

  describe('clear', () => {
    it('should remove all state', () => {
      state.set('key1', 'value1');
      state.set('key2', 'value2');
      expect(state.list().length).toBe(2);
      state.clear();
      expect(state.list().length).toBe(0);
    });

    it('should not throw when state is already empty', () => {
      expect(() => state.clear()).not.toThrow();
      expect(state.list().length).toBe(0);
    });
  });

  describe('key validation', () => {
    it('should reject empty key', () => {
      expect(() => state.get('')).toThrow('empty');
    });

    it('should reject key with forward slash', () => {
      expect(() => state.set('../escape', 'evil')).toThrow('path');
    });

    it('should reject key with backslash', () => {
      expect(() => state.set('..\\escape', 'evil')).toThrow('path');
    });

    it('should reject key with double dots', () => {
      expect(() => state.set('..', 'evil')).toThrow('path');
    });
  });

  describe('complex data', () => {
    it('should handle deeply nested structures', () => {
      const complexData = {
        portfolio: {
          holdings: [
            { symbol: 'AAPL', quantity: 100, price: 150.25 },
            { symbol: 'GOOGL', quantity: 50, price: 2800.0 },
          ],
          total_value: 155025.0,
        },
        signals: [
          { type: 'BUY', symbol: 'AAPL', confidence: 0.85 },
          { type: 'SELL', symbol: 'TSLA', confidence: 0.72 },
        ],
        metadata: {
          last_update: '2024-01-15T10:30:00Z',
          version: 2,
        },
      };
      state.set('complex', complexData);
      const retrieved = state.get('complex');
      expect(retrieved).toEqual(complexData);
    });

    it('should overwrite existing key', () => {
      state.set('key', 'original');
      expect(state.get('key')).toBe('original');
      state.set('key', 'updated');
      expect(state.get('key')).toBe('updated');
    });
  });

  describe('type safety', () => {
    interface Portfolio {
      holdings: { symbol: string; quantity: number }[];
      total: number;
    }

    it('should support typed generics', () => {
      const portfolio: Portfolio = {
        holdings: [{ symbol: 'AAPL', quantity: 100 }],
        total: 15000,
      };
      state.set('portfolio', portfolio);
      const retrieved = state.get<Portfolio>('portfolio');
      expect(retrieved?.holdings[0].symbol).toBe('AAPL');
      expect(retrieved?.total).toBe(15000);
    });
  });
});
