import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, vi, beforeEach, Mock } from 'vitest'
import { CodeEditor } from '../CodeEditor'
import { ArtifactFile } from '@/types'
import { useThemeStore } from '@/stores/themeStore'

// Mock Monaco Editor
vi.mock('@monaco-editor/react', () => ({
  default: vi.fn(({ onMount, onChange, value, language, theme, options }) => {
    // Simulate editor mounting
    if (onMount) {
      const mockEditor = {
        updateOptions: vi.fn(),
        getValue: () => value,
        setValue: vi.fn(),
      }
      onMount(mockEditor)
    }

    return (
      <div data-testid="monaco-editor">
        <div data-testid="editor-language">{language}</div>
        <div data-testid="editor-theme">{theme}</div>
        <div data-testid="editor-readonly">{options?.readOnly ? 'true' : 'false'}</div>
        <textarea
          data-testid="editor-content"
          value={value}
          onChange={e => onChange?.(e.target.value)}
          readOnly={options?.readOnly}
        />
      </div>
    )
  }),
}))

// Mock theme store
vi.mock('@/stores/themeStore', () => ({
  useThemeStore: vi.fn(),
}))

const mockPythonFile: ArtifactFile = {
  id: '1',
  name: 'main.py',
  content: 'print("hello world")',
  language: 'python',
  type: 'file',
}

const mockJavaScriptFile: ArtifactFile = {
  id: '2',
  name: 'script.js',
  content: 'console.log("hello");',
  language: '',
  type: 'file',
}

const mockYamlFile: ArtifactFile = {
  id: '3',
  name: 'config.yaml',
  content: 'debug: true\nport: 3000',
  language: '',
  type: 'file',
}

describe('CodeEditor', () => {
  const mockUseThemeStore = useThemeStore as unknown as Mock

  beforeEach(() => {
    vi.clearAllMocks()
    mockUseThemeStore.mockReturnValue({ isDark: false })
  })

  it('renders editor with file content', () => {
    const onChange = vi.fn()

    render(<CodeEditor file={mockPythonFile} onChange={onChange} />)

    expect(screen.getByText('main.py')).toBeInTheDocument()
    expect(screen.getByTestId('editor-content')).toHaveValue('print("hello world")')
  })

  it('displays correct language from file.language property', () => {
    const onChange = vi.fn()

    render(<CodeEditor file={mockPythonFile} onChange={onChange} />)

    expect(screen.getByTestId('editor-language')).toHaveTextContent('python')
  })

  it('detects language from file extension when language property is empty', () => {
    const onChange = vi.fn()

    render(<CodeEditor file={mockJavaScriptFile} onChange={onChange} />)

    expect(screen.getByTestId('editor-language')).toHaveTextContent('javascript')
  })

  it('detects YAML language from file extension', () => {
    const onChange = vi.fn()

    render(<CodeEditor file={mockYamlFile} onChange={onChange} />)

    expect(screen.getByTestId('editor-language')).toHaveTextContent('yaml')
  })

  it('calls onChange when editor content changes', () => {
    const onChange = vi.fn()

    render(<CodeEditor file={mockPythonFile} onChange={onChange} />)

    const editor = screen.getByTestId('editor-content')

    // Simulate typing in the editor
    const newContent = 'print("modified content")'
    fireEvent.change(editor, { target: { value: newContent } })

    // The mock editor will call onChange directly
    expect(onChange).toHaveBeenCalledWith(newContent)
  })

  it('renders in read-only mode when specified', () => {
    const onChange = vi.fn()

    render(<CodeEditor file={mockPythonFile} onChange={onChange} readOnly={true} />)

    expect(screen.getByTestId('editor-readonly')).toHaveTextContent('true')
    expect(screen.getByTestId('editor-content')).toHaveAttribute('readonly')
  })

  it('renders in editable mode by default', () => {
    const onChange = vi.fn()

    render(<CodeEditor file={mockPythonFile} onChange={onChange} />)

    expect(screen.getByTestId('editor-readonly')).toHaveTextContent('false')
    expect(screen.getByTestId('editor-content')).not.toHaveAttribute('readonly')
  })

  it('uses light theme when isDark is false', () => {
    mockUseThemeStore.mockReturnValue({ isDark: false })
    const onChange = vi.fn()

    render(<CodeEditor file={mockPythonFile} onChange={onChange} />)

    expect(screen.getByTestId('editor-theme')).toHaveTextContent('vs-light')
  })

  it('uses dark theme when isDark is true', () => {
    mockUseThemeStore.mockReturnValue({ isDark: true })
    const onChange = vi.fn()

    render(<CodeEditor file={mockPythonFile} onChange={onChange} />)

    expect(screen.getByTestId('editor-theme')).toHaveTextContent('vs-dark')
  })

  it('handles unknown file extensions gracefully', () => {
    const unknownFile: ArtifactFile = {
      id: '4',
      name: 'unknown.xyz',
      content: 'some content',
      language: '',
      type: 'file',
    }
    const onChange = vi.fn()

    render(<CodeEditor file={unknownFile} onChange={onChange} />)

    expect(screen.getByTestId('editor-language')).toHaveTextContent('plaintext')
  })

  describe('language detection', () => {
    const testCases = [
      { extension: 'py', expected: 'python' },
      { extension: 'js', expected: 'javascript' },
      { extension: 'jsx', expected: 'javascript' },
      { extension: 'ts', expected: 'typescript' },
      { extension: 'tsx', expected: 'typescript' },
      { extension: 'yaml', expected: 'yaml' },
      { extension: 'yml', expected: 'yaml' },
      { extension: 'json', expected: 'json' },
      { extension: 'md', expected: 'markdown' },
      { extension: 'html', expected: 'html' },
      { extension: 'css', expected: 'css' },
      { extension: 'unknown', expected: 'plaintext' },
    ]

    testCases.forEach(({ extension, expected }) => {
      it(`detects ${expected} language for .${extension} files`, () => {
        const file: ArtifactFile = {
          id: '1',
          name: `test.${extension}`,
          content: 'test content',
          language: '',
          type: 'file',
        }
        const onChange = vi.fn()

        render(<CodeEditor file={file} onChange={onChange} />)

        expect(screen.getByTestId('editor-language')).toHaveTextContent(expected)
      })
    })
  })

  it('handles files without extensions', () => {
    const fileWithoutExtension: ArtifactFile = {
      id: '5',
      name: 'Dockerfile',
      content: 'FROM node:18',
      language: '',
      type: 'file',
    }
    const onChange = vi.fn()

    render(<CodeEditor file={fileWithoutExtension} onChange={onChange} />)

    expect(screen.getByTestId('editor-language')).toHaveTextContent('plaintext')
  })

  describe('file selection bug prevention', () => {
    it('uses file.id as key prop to force re-render on file changes', () => {
      const onChange = vi.fn()
      const { rerender } = render(<CodeEditor file={mockPythonFile} onChange={onChange} />)

      // Check initial file
      expect(screen.getByTestId('monaco-editor')).toBeInTheDocument()
      expect(screen.getByTestId('editor-content')).toHaveValue('print("hello world")')

      // Switch to different file
      const newFile: ArtifactFile = {
        id: 'different-id',
        name: 'new.py',
        content: 'print("different content")',
        language: 'python',
        type: 'file',
      }

      rerender(<CodeEditor file={newFile} onChange={onChange} />)

      // Should show new file content
      expect(screen.getByTestId('editor-content')).toHaveValue('print("different content")')
      expect(screen.getByText('new.py')).toBeInTheDocument()
    })

    it('updates editor content when file content changes but same ID', () => {
      const onChange = vi.fn()
      const { rerender } = render(<CodeEditor file={mockPythonFile} onChange={onChange} />)

      // Check initial content
      expect(screen.getByTestId('editor-content')).toHaveValue('print("hello world")')

      // Update file content with same ID
      const updatedFile: ArtifactFile = {
        ...mockPythonFile,
        content: 'print("updated content")',
      }

      rerender(<CodeEditor file={updatedFile} onChange={onChange} />)

      // Should show updated content
      expect(screen.getByTestId('editor-content')).toHaveValue('print("updated content")')
    })

    it('handles rapid file switching correctly', () => {
      const onChange = vi.fn()
      const { rerender } = render(<CodeEditor file={mockPythonFile} onChange={onChange} />)

      const files = [mockJavaScriptFile, mockYamlFile, mockPythonFile]

      // Rapidly switch between files
      files.forEach(file => {
        rerender(<CodeEditor file={file} onChange={onChange} />)
        expect(screen.getByTestId('editor-content')).toHaveValue(file.content)
        expect(screen.getByText(file.name)).toBeInTheDocument()
      })
    })

    it('maintains editor functionality after file switches', () => {
      const onChange = vi.fn()
      const { rerender } = render(<CodeEditor file={mockPythonFile} onChange={onChange} />)

      // Switch to different file
      rerender(<CodeEditor file={mockJavaScriptFile} onChange={onChange} />)

      // Editor should still be functional
      const editor = screen.getByTestId('editor-content')
      const newContent = 'console.log("modified");'
      fireEvent.change(editor, { target: { value: newContent } })

      expect(onChange).toHaveBeenCalledWith(newContent)
    })
  })
})
