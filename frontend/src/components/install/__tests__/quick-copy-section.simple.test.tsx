import React from 'react';
import { render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import { QuickCopySection, CodeBlock } from '../quick-copy-section';

describe('QuickCopySection - Simple Tests', () => {
  const mockCommand = 'curl -fsSL https://example.com/install.sh | bash';
  const mockPlatform = 'macOS (Intel)';

  it('should render command and platform', () => {
    render(<QuickCopySection command={mockCommand} platform={mockPlatform} />);

    expect(screen.getByText(mockCommand)).toBeInTheDocument();
    expect(screen.getByText('Quick Install')).toBeInTheDocument();
    expect(screen.getByText('macOS (Intel)')).toBeInTheDocument();
  });

  it('should render without platform when not provided', () => {
    render(<QuickCopySection command={mockCommand} />);

    expect(screen.getByText(mockCommand)).toBeInTheDocument();
    expect(screen.getByText('Quick Install')).toBeInTheDocument();
    expect(screen.queryByText('macOS (Intel)')).not.toBeInTheDocument();
  });

  it('should have copy button', () => {
    render(<QuickCopySection command={mockCommand} platform={mockPlatform} />);

    // Look for button containing Copy icon or text
    const buttons = screen.getAllByRole('button');
    expect(buttons.length).toBeGreaterThan(0);
  });

  it('should display description text', () => {
    render(<QuickCopySection command={mockCommand} platform={mockPlatform} />);

    expect(
      screen.getByText(/Copy and paste this command into your terminal/),
    ).toBeInTheDocument();
    expect(
      screen.getByText(/Paste this command in your terminal and press Enter/),
    ).toBeInTheDocument();
  });
});

describe('CodeBlock - Simple Tests', () => {
  const mockCode = 'npm install -g @theo/cli';

  it('should render code content', () => {
    render(<CodeBlock code={mockCode} />);

    const codeElement = screen.getByText(mockCode);
    expect(codeElement).toBeInTheDocument();
    expect(codeElement.tagName).toBe('CODE');
  });

  it('should render with proper container styling', () => {
    render(<CodeBlock code={mockCode} />);

    const preElement = screen.getByText(mockCode).closest('pre');
    expect(preElement).toHaveClass('font-mono');
    expect(preElement).toHaveClass('bg-muted');
  });

  it('should show copy button when showCopy is true', () => {
    render(<CodeBlock code={mockCode} showCopy={true} />);

    const buttons = screen.getAllByRole('button');
    expect(buttons.length).toBe(1);
  });

  it('should hide copy button when showCopy is false', () => {
    render(<CodeBlock code={mockCode} showCopy={false} />);

    const buttons = screen.queryAllByRole('button');
    expect(buttons.length).toBe(0);
  });

  it('should default to showing copy button', () => {
    render(<CodeBlock code={mockCode} />);

    const buttons = screen.getAllByRole('button');
    expect(buttons.length).toBe(1);
  });

  it('should handle multiline code properly', () => {
    const multilineCode = `git clone https://github.com/the0-org/cli.git
cd cli
make build`;

    render(<CodeBlock code={multilineCode} />);

    // Check for parts of the multiline code since whitespace normalization happens
    expect(screen.getByText(/git clone/)).toBeInTheDocument();
    expect(screen.getByText(/cd cli/)).toBeInTheDocument();
    expect(screen.getByText(/make build/)).toBeInTheDocument();
  });

  it('should handle empty code gracefully', () => {
    render(<CodeBlock code="" />);

    const buttons = screen.getAllByRole('button');
    expect(buttons.length).toBe(1);
  });
});
