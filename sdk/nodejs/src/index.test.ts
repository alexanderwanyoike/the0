/**
 * Tests for the0 Node.js SDK
 */

import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import * as fs from 'fs';
import * as path from 'path';

// We need to test the actual implementation
// Import functions directly
import { parse, success, error, result, metric, log, sleep } from './index';

describe('parse', () => {
  const originalEnv = process.env;

  beforeEach(() => {
    vi.resetModules();
    process.env = { ...originalEnv };
  });

  afterEach(() => {
    process.env = originalEnv;
  });

  it('should parse valid configuration', () => {
    process.env.BOT_ID = 'test-bot-123';
    process.env.BOT_CONFIG = JSON.stringify({ symbol: 'BTC/USDT', amount: 100.5 });

    const { id, config } = parse();

    expect(id).toBe('test-bot-123');
    expect(config.symbol).toBe('BTC/USDT');
    expect(config.amount).toBe(100.5);
  });

  it('should throw when BOT_ID is missing', () => {
    delete process.env.BOT_ID;
    process.env.BOT_CONFIG = '{}';

    expect(() => parse()).toThrow('BOT_ID');
  });

  it('should throw when BOT_CONFIG is missing', () => {
    process.env.BOT_ID = 'test';
    delete process.env.BOT_CONFIG;

    expect(() => parse()).toThrow('BOT_CONFIG');
  });

  it('should throw on invalid JSON', () => {
    process.env.BOT_ID = 'test';
    process.env.BOT_CONFIG = 'not valid json';

    expect(() => parse()).toThrow('JSON');
  });

  it('should parse nested configuration', () => {
    const configData = {
      symbol: 'ETH/USD',
      credentials: { api_key: 'key123', secret: 'secret456' },
      settings: { paper: true, amount: 50.0 },
    };
    process.env.BOT_ID = 'nested-bot';
    process.env.BOT_CONFIG = JSON.stringify(configData);

    const { id, config } = parse<typeof configData>();

    expect(config.credentials.api_key).toBe('key123');
    expect(config.settings.paper).toBe(true);
  });

  it('should support typed generics', () => {
    interface MyConfig {
      symbol: string;
      amount: number;
    }
    process.env.BOT_ID = 'typed-bot';
    process.env.BOT_CONFIG = JSON.stringify({ symbol: 'AAPL', amount: 200 });

    const { config } = parse<MyConfig>();

    // TypeScript should infer these types
    expect(config.symbol).toBe('AAPL');
    expect(config.amount).toBe(200);
  });
});

describe('success', () => {
  const originalEnv = process.env;
  let tempDir: string;

  beforeEach(() => {
    tempDir = fs.mkdtempSync('/tmp/the0-test-');
    process.env = { ...originalEnv, CODE_MOUNT_DIR: tempDir };
  });

  afterEach(() => {
    process.env = originalEnv;
    fs.rmSync(tempDir, { recursive: true, force: true });
  });

  it('should write simple success result', () => {
    success('Trade completed');

    const resultPath = path.join('/', tempDir, 'result.json');
    const data = JSON.parse(fs.readFileSync(resultPath, 'utf-8'));

    expect(data.status).toBe('success');
    expect(data.message).toBe('Trade completed');
  });

  it('should write success with additional data', () => {
    success('Order filled', { order_id: '123', filled: 0.5 });

    const resultPath = path.join('/', tempDir, 'result.json');
    const data = JSON.parse(fs.readFileSync(resultPath, 'utf-8'));

    expect(data.status).toBe('success');
    expect(data.data.order_id).toBe('123');
    expect(data.data.filled).toBe(0.5);
  });
});

describe('error', () => {
  const originalEnv = process.env;
  const originalExit = process.exit;
  let tempDir: string;

  beforeEach(() => {
    tempDir = fs.mkdtempSync('/tmp/the0-test-');
    process.env = { ...originalEnv, CODE_MOUNT_DIR: tempDir };
    // Mock process.exit
    process.exit = vi.fn() as never;
  });

  afterEach(() => {
    process.env = originalEnv;
    process.exit = originalExit;
    fs.rmSync(tempDir, { recursive: true, force: true });
  });

  it('should exit with code 1', () => {
    error('Something went wrong');

    expect(process.exit).toHaveBeenCalledWith(1);
  });

  it('should write error result', () => {
    error('Connection failed');

    const resultPath = path.join('/', tempDir, 'result.json');
    const data = JSON.parse(fs.readFileSync(resultPath, 'utf-8'));

    expect(data.status).toBe('error');
    expect(data.message).toBe('Connection failed');
  });

  it('should write error with additional data', () => {
    error('Insufficient funds', { available: 100, required: 150 });

    const resultPath = path.join('/', tempDir, 'result.json');
    const data = JSON.parse(fs.readFileSync(resultPath, 'utf-8'));

    expect(data.data.available).toBe(100);
    expect(data.data.required).toBe(150);
  });
});

describe('result', () => {
  const originalEnv = process.env;
  let tempDir: string;

  beforeEach(() => {
    tempDir = fs.mkdtempSync('/tmp/the0-test-');
    process.env = { ...originalEnv, CODE_MOUNT_DIR: tempDir };
  });

  afterEach(() => {
    process.env = originalEnv;
    fs.rmSync(tempDir, { recursive: true, force: true });
  });

  it('should write custom result data', () => {
    result({
      status: 'success',
      trade_id: 'abc123',
      signals: [{ symbol: 'BTC', direction: 'long' }],
    });

    const resultPath = path.join('/', tempDir, 'result.json');
    const data = JSON.parse(fs.readFileSync(resultPath, 'utf-8'));

    expect(data.trade_id).toBe('abc123');
    expect(data.signals).toHaveLength(1);
    expect(data.signals[0].direction).toBe('long');
  });
});

describe('metric', () => {
  let consoleSpy: ReturnType<typeof vi.spyOn>;

  beforeEach(() => {
    consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
  });

  afterEach(() => {
    consoleSpy.mockRestore();
  });

  it('should output metric as JSON to stdout', () => {
    metric('price', { symbol: 'BTC/USD', value: 45000 });

    expect(consoleSpy).toHaveBeenCalled();
    const output = consoleSpy.mock.calls[0][0];
    const data = JSON.parse(output);

    expect(data._metric).toBe('price');
    expect(data.symbol).toBe('BTC/USD');
    expect(data.value).toBe(45000);
  });

  it('should include ISO timestamp', () => {
    metric('signal', { direction: 'long' });

    const output = consoleSpy.mock.calls[0][0];
    const data = JSON.parse(output);

    expect(data.timestamp).toBeDefined();
    // Should be a valid ISO date string
    expect(new Date(data.timestamp).toISOString()).toBe(data.timestamp);
  });
});

describe('log', () => {
  let consoleSpy: ReturnType<typeof vi.spyOn>;

  beforeEach(() => {
    consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
  });

  afterEach(() => {
    consoleSpy.mockRestore();
  });

  it('should log simple message', () => {
    log('Starting trade execution');

    expect(consoleSpy).toHaveBeenCalledWith('Starting trade execution');
  });

  it('should log message with structured data', () => {
    log('Order placed', { order_id: '123', symbol: 'BTC' });

    const output = consoleSpy.mock.calls[0][0];
    const data = JSON.parse(output);

    expect(data.message).toBe('Order placed');
    expect(data.order_id).toBe('123');
    expect(data.symbol).toBe('BTC');
  });
});

describe('sleep', () => {
  it('should sleep for specified duration', async () => {
    const start = Date.now();
    await sleep(100); // 100ms
    const elapsed = Date.now() - start;

    // Should be at least 100ms
    expect(elapsed).toBeGreaterThanOrEqual(95);
    // But not too much longer
    expect(elapsed).toBeLessThan(200);
  });
});
