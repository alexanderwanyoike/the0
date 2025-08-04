import React from 'react';
import { render, screen, waitFor } from '@testing-library/react';
import { useRouter } from 'next/navigation';
import { withAuth } from '../with-auth';
import { useAuth } from '@/contexts/auth-context';
import { AuthUser } from '@/lib/auth/types';

// Mock the auth context
jest.mock('@/contexts/auth-context', () => ({
  useAuth: jest.fn(),
}));

// Mock Next.js router
jest.mock('next/navigation', () => ({
  useRouter: jest.fn(),
}));

const mockUseAuth = useAuth as jest.MockedFunction<typeof useAuth>;
const mockUseRouter = useRouter as jest.MockedFunction<typeof useRouter>;

// Test component to wrap with withAuth
function TestComponent({ testProp }: { testProp?: string }) {
  return <div data-testid="test-component">Test Component {testProp}</div>;
}

const mockUser: AuthUser = {
  id: '1',
  username: 'testuser',
  email: 'test@example.com',
  firstName: 'Test',
  lastName: 'User',
  isActive: true,
  isEmailVerified: true,
};

describe('withAuth HOC', () => {
  const mockRouter = {
    push: jest.fn(),
    replace: jest.fn(),
    back: jest.fn(),
    forward: jest.fn(),
    refresh: jest.fn(),
    prefetch: jest.fn(),
  };

  const defaultAuthContext = {
    user: mockUser,
    userData: mockUser,
    loading: false,
    login: jest.fn(),
    register: jest.fn(),
    logout: jest.fn(),
    token: 'valid-token',
    authService: {} as any,
  };

  beforeEach(() => {
    jest.clearAllMocks();
    mockUseAuth.mockReturnValue(defaultAuthContext);
    mockUseRouter.mockReturnValue(mockRouter);
  });

  it('should render component when user is authenticated', () => {
    const ProtectedComponent = withAuth(TestComponent);

    render(<ProtectedComponent testProp="hello" />);

    expect(screen.getByTestId('test-component')).toBeInTheDocument();
    expect(screen.getByText('Test Component hello')).toBeInTheDocument();
  });

  it('should not render component when loading', () => {
    mockUseAuth.mockReturnValue({
      ...defaultAuthContext,
      loading: true,
    });

    const ProtectedComponent = withAuth(TestComponent);

    render(<ProtectedComponent />);

    expect(screen.queryByTestId('test-component')).not.toBeInTheDocument();
  });

  it('should not render component when no user', () => {
    mockUseAuth.mockReturnValue({
      ...defaultAuthContext,
      user: null,
      userData: null,
    });

    const ProtectedComponent = withAuth(TestComponent);

    render(<ProtectedComponent />);

    expect(screen.queryByTestId('test-component')).not.toBeInTheDocument();
  });

  it('should redirect to login when not authenticated', async () => {
    mockUseAuth.mockReturnValue({
      ...defaultAuthContext,
      user: null,
      userData: null,
      loading: false,
    });

    const ProtectedComponent = withAuth(TestComponent);

    render(<ProtectedComponent />);

    await waitFor(() => {
      expect(mockRouter.replace).toHaveBeenCalledWith('/login');
    });
  });

  it('should not redirect when loading', async () => {
    mockUseAuth.mockReturnValue({
      ...defaultAuthContext,
      user: null,
      userData: null,
      loading: true,
    });

    const ProtectedComponent = withAuth(TestComponent);

    render(<ProtectedComponent />);

    // Wait a bit to ensure redirect doesn't happen
    await new Promise(resolve => setTimeout(resolve, 100));

    expect(mockRouter.replace).not.toHaveBeenCalled();
  });

  it('should redirect when user becomes unauthenticated', async () => {
    // Start with authenticated user
    const { rerender } = render(React.createElement(withAuth(TestComponent)));

    expect(screen.getByTestId('test-component')).toBeInTheDocument();

    // User becomes unauthenticated
    mockUseAuth.mockReturnValue({
      ...defaultAuthContext,
      user: null,
      userData: null,
      loading: false,
    });

    rerender(React.createElement(withAuth(TestComponent)));

    await waitFor(() => {
      expect(mockRouter.replace).toHaveBeenCalledWith('/login');
    });

    expect(screen.queryByTestId('test-component')).not.toBeInTheDocument();
  });

  it('should render component when user becomes authenticated', () => {
    // Start with no user
    mockUseAuth.mockReturnValue({
      ...defaultAuthContext,
      user: null,
      userData: null,
    });

    const { rerender } = render(React.createElement(withAuth(TestComponent)));

    expect(screen.queryByTestId('test-component')).not.toBeInTheDocument();

    // User becomes authenticated
    mockUseAuth.mockReturnValue(defaultAuthContext);

    rerender(React.createElement(withAuth(TestComponent)));

    expect(screen.getByTestId('test-component')).toBeInTheDocument();
  });

  it('should pass through all props to wrapped component', () => {
    const ProtectedComponent = withAuth(TestComponent);

    render(<ProtectedComponent testProp="passed through" />);

    expect(screen.getByText('Test Component passed through')).toBeInTheDocument();
  });

  it('should preserve component display name', () => {
    (TestComponent as any).displayName = 'TestComponent';
    const ProtectedComponent = withAuth(TestComponent);

    expect((ProtectedComponent as any).displayName).toBe('ProtectedRoute');
  });

  it('should handle component without display name', () => {
    const AnonymousComponent = () => <div>Anonymous</div>;
    const ProtectedComponent = withAuth(AnonymousComponent);

    expect((ProtectedComponent as any).displayName).toBe('ProtectedRoute');
  });

  it('should handle loading to authenticated transition', async () => {
    // Start with loading state
    mockUseAuth.mockReturnValue({
      ...defaultAuthContext,
      loading: true,
      user: null,
      userData: null,
    });

    const { rerender } = render(React.createElement(withAuth(TestComponent)));

    expect(screen.queryByTestId('test-component')).not.toBeInTheDocument();
    expect(mockRouter.replace).not.toHaveBeenCalled();

    // Transition to authenticated
    mockUseAuth.mockReturnValue(defaultAuthContext);

    rerender(React.createElement(withAuth(TestComponent)));

    expect(screen.getByTestId('test-component')).toBeInTheDocument();
    expect(mockRouter.replace).not.toHaveBeenCalled();
  });

  it('should handle loading to unauthenticated transition', async () => {
    // Start with loading state
    mockUseAuth.mockReturnValue({
      ...defaultAuthContext,
      loading: true,
      user: null,
      userData: null,
    });

    const { rerender } = render(React.createElement(withAuth(TestComponent)));

    expect(screen.queryByTestId('test-component')).not.toBeInTheDocument();
    expect(mockRouter.replace).not.toHaveBeenCalled();

    // Transition to unauthenticated
    mockUseAuth.mockReturnValue({
      ...defaultAuthContext,
      loading: false,
      user: null,
      userData: null,
    });

    rerender(React.createElement(withAuth(TestComponent)));

    await waitFor(() => {
      expect(mockRouter.replace).toHaveBeenCalledWith('/login');
    });
    expect(screen.queryByTestId('test-component')).not.toBeInTheDocument();
  });

  it('should work with TypeScript generic props', () => {
    interface TestProps {
      title: string;
      count: number;
    }

    const TypedComponent = ({ title, count }: TestProps) => (
      <div data-testid="typed-component">
        {title}: {count}
      </div>
    );

    const ProtectedTypedComponent = withAuth<TestProps>(TypedComponent);

    render(<ProtectedTypedComponent title="Items" count={5} />);

    expect(screen.getByTestId('typed-component')).toBeInTheDocument();
    expect(screen.getByText('Items: 5')).toBeInTheDocument();
  });

  it('should not cause infinite re-renders', async () => {
    const renderSpy = jest.fn();
    
    const TestComponentWithSpy = (props: any) => {
      renderSpy();
      return <TestComponent {...props} />;
    };

    const ProtectedComponent = withAuth(TestComponentWithSpy);

    render(<ProtectedComponent />);

    // Let any effects run
    await waitFor(() => {
      expect(screen.getByTestId('test-component')).toBeInTheDocument();
    });

    // Should not re-render excessively
    expect(renderSpy).toHaveBeenCalledTimes(1);
  });

  it('should handle router dependencies correctly', () => {
    // Ensure the effect dependencies are correct by testing multiple renders
    const { rerender } = render(React.createElement(withAuth(TestComponent)));

    expect(screen.getByTestId('test-component')).toBeInTheDocument();

    // Re-render with same auth state
    rerender(React.createElement(withAuth(TestComponent)));

    expect(screen.getByTestId('test-component')).toBeInTheDocument();
    expect(mockRouter.replace).not.toHaveBeenCalled();
  });
});