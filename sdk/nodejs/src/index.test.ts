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
  let stderrSpy: ReturnType<typeof vi.spyOn>;

  beforeEach(() => {
    stderrSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
  });

  afterEach(() => {
    stderrSpy.mockRestore();
  });

  it('should output JSON with message field to stderr', () => {
    log('Starting bot execution');

    expect(stderrSpy).toHaveBeenCalled();
    const output = JSON.parse(stderrSpy.mock.calls[0][0]);

    expect(output.message).toBe('Starting bot execution');
  });

  it('should default to info level', () => {
    log('Test message');

    const output = JSON.parse(stderrSpy.mock.calls[0][0]);

    expect(output.level).toBe('info');
  });

  it('should include timestamp', () => {
    log('Test message');

    const output = JSON.parse(stderrSpy.mock.calls[0][0]);

    expect(output.timestamp).toBeDefined();
    expect(new Date(output.timestamp).toISOString()).toBe(output.timestamp);
  });

  it('should support warn level as second argument', () => {
    log('Warning message', 'warn');

    const output = JSON.parse(stderrSpy.mock.calls[0][0]);

    expect(output.level).toBe('warn');
    expect(output.message).toBe('Warning message');
  });

  it('should support error level as second argument', () => {
    log('Error message', 'error');

    const output = JSON.parse(stderrSpy.mock.calls[0][0]);

    expect(output.level).toBe('error');
  });

  it('should merge data fields (pino-style)', () => {
    log('Order placed', { orderId: '12345', symbol: 'BTC' });

    const output = JSON.parse(stderrSpy.mock.calls[0][0]);

    expect(output.message).toBe('Order placed');
    expect(output.orderId).toBe('12345');
    expect(output.symbol).toBe('BTC');
    expect(output.level).toBe('info');
  });

  it('should support data and level together', () => {
    log('Order failed', { orderId: '12345' }, 'error');

    const output = JSON.parse(stderrSpy.mock.calls[0][0]);

    expect(output.level).toBe('error');
    expect(output.message).toBe('Order failed');
    expect(output.orderId).toBe('12345');
  });

  it('should handle empty message', () => {
    log('');

    const output = JSON.parse(stderrSpy.mock.calls[0][0]);

    expect(output.message).toBe('');
  });

  it('should handle special characters in message', () => {
    log('Error: "file not found" at C:\\path');

    const output = JSON.parse(stderrSpy.mock.calls[0][0]);

    expect(output.message).toBe('Error: "file not found" at C:\\path');
  });

  it('should handle newlines in message', () => {
    log('Line 1\nLine 2\nLine 3');

    const output = JSON.parse(stderrSpy.mock.calls[0][0]);

    expect(output.message).toBe('Line 1\nLine 2\nLine 3');
  });

  it('should handle long message', () => {
    const longMessage = 'x'.repeat(10000);
    log(longMessage);

    const output = JSON.parse(stderrSpy.mock.calls[0][0]);

    expect(output.message).toBe(longMessage);
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
