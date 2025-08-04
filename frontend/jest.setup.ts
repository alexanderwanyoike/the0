import '@testing-library/jest-dom';

// Polyfill for userEvent
import 'whatwg-fetch';
import { TextEncoder, TextDecoder } from 'util';
global.TextEncoder = TextEncoder;
//@ts-ignore
global.TextDecoder = TextDecoder;

// Mock matchMedia for tests
Object.defineProperty(window, 'matchMedia', {
  writable: true,
  value: jest.fn().mockImplementation((query) => ({
    matches: false,
    media: query,
    onchange: null,
    addListener: jest.fn(), // deprecated
    removeListener: jest.fn(), // deprecated
    addEventListener: jest.fn(),
    removeEventListener: jest.fn(),
    dispatchEvent: jest.fn(),
  })),
});

// Setup MSW for API mocking
import { setupServer } from 'msw/node';
import { http, HttpResponse } from 'msw';

// API mocking setup
const server = setupServer(
  http.get('/api/users/:id', ({ params }) => {
    return HttpResponse.json({
      success: true,
      data: {
        id: params.id,
        email: 'test@example.com',
        displayName: 'Test User',
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
        isActive: true,
        isEmailVerified: true,
      }
    });
  }),
  
  http.post('/api/users', () => {
    return HttpResponse.json({
      success: true,
      data: { 
        id: 'new-user-id', 
        email: 'test@example.com',
        displayName: 'Test User',
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
        isActive: true,
        isEmailVerified: true,
      }
    });
  }),

  http.put('/api/users/:id', () => {
    return HttpResponse.json({
      success: true,
      data: { 
        id: 'user-id', 
        email: 'test@example.com',
        displayName: 'Updated User',
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
        isActive: true,
        isEmailVerified: true,
      }
    });
  }),

  http.delete('/api/users/:id', () => {
    return HttpResponse.json({ success: true });
  }),
);

beforeAll(() => server.listen());
afterEach(() => server.resetHandlers());
afterAll(() => server.close());

// Mock Next.js router
jest.mock('next/navigation', () => ({
  useRouter: () => ({
    push: jest.fn(),
    replace: jest.fn(),
    refresh: jest.fn(),
    back: jest.fn(),
    forward: jest.fn(),
    prefetch: jest.fn(),
  }),
  usePathname: () => '/test-path',
  useSearchParams: () => new URLSearchParams(),
  useParams: () => ({}),
}));

// Mock Next.js Image component
jest.mock('next/image', () => ({
  __esModule: true,
  default: function Image(props: any) {
    return props;
  },
}));

// Mock environment variables commonly used in tests
process.env.NEXT_PUBLIC_API_URL = 'http://localhost:3001';
process.env.JWT_SECRET = 'test-jwt-secret-key-for-testing-only';

// Mock console methods to reduce noise in tests
const originalError = console.error;
const originalWarn = console.warn;

beforeAll(() => {
  console.error = jest.fn((...args) => {
    // Only show errors that aren't React act() warnings
    if (
      typeof args[0] === 'string' &&
      !args[0].includes('Warning: An update to') &&
      !args[0].includes('act()')
    ) {
      originalError(...args);
    }
  });
  console.warn = jest.fn((...args) => {
    // Filter out specific warnings if needed
    originalWarn(...args);
  });
});

afterAll(() => {
  console.error = originalError;
  console.warn = originalWarn;
});
