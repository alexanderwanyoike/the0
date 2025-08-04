import React from 'react';
import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { useRouter } from 'next/navigation';
import { RegisterForm } from '../register-form';
import { useAuth } from '@/contexts/auth-context';

// Mock the auth context
jest.mock('@/contexts/auth-context', () => ({
  useAuth: jest.fn(),
}));

// Mock Next.js router
jest.mock('next/navigation', () => ({
  useRouter: jest.fn(),
}));

// Mock the constants
jest.mock('@/lib/constants', () => ({
  APP_NAME: 'Test App',
}));

const mockUseAuth = useAuth as jest.MockedFunction<typeof useAuth>;
const mockUseRouter = useRouter as jest.MockedFunction<typeof useRouter>;

describe('RegisterForm', () => {
  const mockRegister = jest.fn();
  const mockRouter = {
    push: jest.fn(),
    replace: jest.fn(),
    back: jest.fn(),
    forward: jest.fn(),
    refresh: jest.fn(),
    prefetch: jest.fn(),
  };
  
  const defaultAuthContext = {
    user: null,
    userData: null,
    loading: false,
    login: jest.fn(),
    register: mockRegister,
    logout: jest.fn(),
    token: null,
    authService: {} as any,
  };

  beforeEach(() => {
    jest.clearAllMocks();
    mockUseAuth.mockReturnValue(defaultAuthContext);
    mockUseRouter.mockReturnValue(mockRouter);
  });

  it('should render register form with all fields', () => {
    render(<RegisterForm />);

    expect(screen.getByText('Test App Registration')).toBeInTheDocument();
    expect(screen.getByText('Create a new account to get started')).toBeInTheDocument();
    expect(screen.getByLabelText('Email')).toBeInTheDocument();
    expect(screen.getByLabelText('Password')).toBeInTheDocument();
    expect(screen.getByLabelText('Confirm Password')).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Create Account' })).toBeInTheDocument();
  });

  it('should handle successful registration', async () => {
    const user = userEvent.setup();
    mockRegister.mockResolvedValue({ success: true });

    render(<RegisterForm />);

    const emailInput = screen.getByLabelText('Email');
    const passwordInput = screen.getByLabelText('Password');
    const confirmPasswordInput = screen.getByLabelText('Confirm Password');
    const registerButton = screen.getByRole('button', { name: 'Create Account' });

    await user.type(emailInput, 'test@example.com');
    await user.type(passwordInput, 'password123');
    await user.type(confirmPasswordInput, 'password123');
    await user.click(registerButton);

    await waitFor(() => {
      expect(mockRegister).toHaveBeenCalledWith({
        username: 'test', // derived from email
        email: 'test@example.com',
        password: 'password123',
      });
    });

    // Should not show any error
    expect(screen.queryByRole('alert')).not.toBeInTheDocument();
  });

  it('should handle failed registration with error message', async () => {
    const user = userEvent.setup();
    mockRegister.mockResolvedValue({ 
      success: false, 
      error: 'User already exists' 
    });

    render(<RegisterForm />);

    const emailInput = screen.getByLabelText('Email');
    const passwordInput = screen.getByLabelText('Password');
    const confirmPasswordInput = screen.getByLabelText('Confirm Password');
    const registerButton = screen.getByRole('button', { name: 'Create Account' });

    await user.type(emailInput, 'existing@example.com');
    await user.type(passwordInput, 'password123');
    await user.type(confirmPasswordInput, 'password123');
    await user.click(registerButton);

    await waitFor(() => {
      expect(screen.getByText('User already exists')).toBeInTheDocument();
    });
  });

  it('should validate password confirmation', async () => {
    const user = userEvent.setup();

    render(<RegisterForm />);

    const emailInput = screen.getByLabelText('Email');
    const passwordInput = screen.getByLabelText('Password');
    const confirmPasswordInput = screen.getByLabelText('Confirm Password');
    const registerButton = screen.getByRole('button', { name: 'Create Account' });

    await user.type(emailInput, 'test@example.com');
    await user.type(passwordInput, 'password123');
    await user.type(confirmPasswordInput, 'differentpassword');
    await user.click(registerButton);

    await waitFor(() => {
      expect(screen.getByText('Passwords do not match')).toBeInTheDocument();
    });

    // Should not call register
    expect(mockRegister).not.toHaveBeenCalled();
  });

  it('should handle network error during registration', async () => {
    const user = userEvent.setup();
    mockRegister.mockRejectedValue(new Error('Network error'));

    render(<RegisterForm />);

    const emailInput = screen.getByLabelText('Email');
    const passwordInput = screen.getByLabelText('Password');
    const confirmPasswordInput = screen.getByLabelText('Confirm Password');
    const registerButton = screen.getByRole('button', { name: 'Create Account' });

    await user.type(emailInput, 'test@example.com');
    await user.type(passwordInput, 'password123');
    await user.type(confirmPasswordInput, 'password123');
    await user.click(registerButton);

    await waitFor(() => {
      expect(screen.getByText('Network error')).toBeInTheDocument();
    });
  });

  it('should show loading state during registration', async () => {
    const user = userEvent.setup();
    let resolveRegister: (value: any) => void;
    const registerPromise = new Promise((resolve) => {
      resolveRegister = resolve;
    });
    mockRegister.mockReturnValue(registerPromise);

    render(<RegisterForm />);

    const emailInput = screen.getByLabelText('Email');
    const passwordInput = screen.getByLabelText('Password');
    const confirmPasswordInput = screen.getByLabelText('Confirm Password');
    const registerButton = screen.getByRole('button', { name: 'Create Account' });

    await user.type(emailInput, 'test@example.com');
    await user.type(passwordInput, 'password123');
    await user.type(confirmPasswordInput, 'password123');
    await user.click(registerButton);

    // Should show loading state
    expect(screen.getByText('Creating account...')).toBeInTheDocument();
    expect(registerButton).toBeDisabled();

    // Resolve the registration
    resolveRegister!({ success: true });

    await waitFor(() => {
      expect(screen.getByText('Create Account')).toBeInTheDocument();
      expect(registerButton).not.toBeDisabled();
    });
  });

  it('should generate username from email', async () => {
    const user = userEvent.setup();
    mockRegister.mockResolvedValue({ success: true });

    render(<RegisterForm />);

    const emailInput = screen.getByLabelText('Email');
    const passwordInput = screen.getByLabelText('Password');
    const confirmPasswordInput = screen.getByLabelText('Confirm Password');
    const registerButton = screen.getByRole('button', { name: 'Create Account' });

    await user.type(emailInput, 'john.doe@company.com');
    await user.type(passwordInput, 'password123');
    await user.type(confirmPasswordInput, 'password123');
    await user.click(registerButton);

    await waitFor(() => {
      expect(mockRegister).toHaveBeenCalledWith({
        username: 'john.doe', // extracted from email
        email: 'john.doe@company.com',
        password: 'password123',
      });
    });
  });

  it('should have proper form validation', async () => {
    const user = userEvent.setup();
    
    render(<RegisterForm />);

    const emailInput = screen.getByLabelText('Email');
    const passwordInput = screen.getByLabelText('Password');
    const confirmPasswordInput = screen.getByLabelText('Confirm Password');
    const registerButton = screen.getByRole('button', { name: 'Create Account' });

    // Try to submit without filling fields
    await user.click(registerButton);

    // HTML5 validation should prevent submission
    expect(mockRegister).not.toHaveBeenCalled();

    // Fields should be required
    expect(emailInput).toBeRequired();
    expect(passwordInput).toBeRequired();
    expect(confirmPasswordInput).toBeRequired();
    expect(emailInput).toHaveAttribute('type', 'email');
    expect(passwordInput).toHaveAttribute('type', 'password');
    expect(confirmPasswordInput).toHaveAttribute('type', 'password');
  });

  it('should have login navigation link', async () => {
    const user = userEvent.setup();

    render(<RegisterForm />);

    const loginLink = screen.getByText('Login here');
    expect(loginLink).toBeInTheDocument();

    await user.click(loginLink);

    expect(mockRouter.push).toHaveBeenCalledWith('/login');
  });

  it('should handle Firebase-style error formatting', async () => {
    const user = userEvent.setup();
    
    // Mock error with Firebase-style message
    const firebaseError = new Error('auth/email-already-in-use');
    mockRegister.mockRejectedValue(firebaseError);

    render(<RegisterForm />);

    const emailInput = screen.getByLabelText('Email');
    const passwordInput = screen.getByLabelText('Password');
    const confirmPasswordInput = screen.getByLabelText('Confirm Password');
    const registerButton = screen.getByRole('button', { name: 'Create Account' });

    await user.type(emailInput, 'existing@example.com');
    await user.type(passwordInput, 'password123');
    await user.type(confirmPasswordInput, 'password123');
    await user.click(registerButton);

    await waitFor(() => {
      expect(screen.getByText('This email is already registered. Please try logging in instead.')).toBeInTheDocument();
    });
  });

  it('should handle weak password error', async () => {
    const user = userEvent.setup();
    
    const weakPasswordError = new Error('auth/weak-password');
    mockRegister.mockRejectedValue(weakPasswordError);

    render(<RegisterForm />);

    const emailInput = screen.getByLabelText('Email');
    const passwordInput = screen.getByLabelText('Password');
    const confirmPasswordInput = screen.getByLabelText('Confirm Password');
    const registerButton = screen.getByRole('button', { name: 'Create Account' });

    await user.type(emailInput, 'test@example.com');
    await user.type(passwordInput, '123');
    await user.type(confirmPasswordInput, '123');
    await user.click(registerButton);

    await waitFor(() => {
      expect(screen.getByText('Password should be at least 6 characters long.')).toBeInTheDocument();
    });
  });

  it('should handle invalid email error', async () => {
    const user = userEvent.setup();
    
    const invalidEmailError = new Error('auth/invalid-email');
    mockRegister.mockRejectedValue(invalidEmailError);

    render(<RegisterForm />);

    const emailInput = screen.getByLabelText('Email');
    const passwordInput = screen.getByLabelText('Password');
    const confirmPasswordInput = screen.getByLabelText('Confirm Password');
    const registerButton = screen.getByRole('button', { name: 'Create Account' });

    // Use a valid email format that passes HTML5 validation but triggers Firebase error
    await user.type(emailInput, 'valid@format.com');
    await user.type(passwordInput, 'password123');
    await user.type(confirmPasswordInput, 'password123');
    await user.click(registerButton);

    await waitFor(() => {
      expect(screen.getByText('Please enter a valid email address.')).toBeInTheDocument();
    });
  });

  it('should clear errors when user retries', async () => {
    const user = userEvent.setup();
    mockRegister.mockResolvedValueOnce({ 
      success: false, 
      error: 'Registration failed' 
    });

    render(<RegisterForm />);

    const emailInput = screen.getByLabelText('Email');
    const passwordInput = screen.getByLabelText('Password');
    const confirmPasswordInput = screen.getByLabelText('Confirm Password');
    const registerButton = screen.getByRole('button', { name: 'Create Account' });

    // First, trigger an error
    await user.type(emailInput, 'test@example.com');
    await user.type(passwordInput, 'password123');
    await user.type(confirmPasswordInput, 'password123');
    await user.click(registerButton);

    await waitFor(() => {
      expect(screen.getByText('Registration failed')).toBeInTheDocument();
    });

    // Now try again - error should be cleared
    mockRegister.mockResolvedValue({ success: true });
    await user.click(registerButton);

    // Error should be cleared during loading
    expect(screen.queryByText('Registration failed')).not.toBeInTheDocument();
  });
});