import { render, screen } from '@testing-library/react'
import { describe, it, expect, vi } from 'vitest'
import { FileTree } from '../FileTree'
import { ArtifactFile } from '@/types'

const mockFiles: ArtifactFile[] = [
  {
    id: '1',
    name: 'main.py',
    content: 'print("hello")',
    language: 'python',
    type: 'file',
  },
  {
    id: '2',
    name: 'config.yaml',
    content: 'debug: true',
    language: 'yaml',
    type: 'file',
  },
]

describe('FileTree', () => {
  it('renders all files', () => {
    const onFileSelect = vi.fn()

    render(<FileTree files={mockFiles} selectedFile={null} onFileSelect={onFileSelect} />)

    expect(screen.getByText('main.py')).toBeInTheDocument()
    expect(screen.getByText('config.yaml')).toBeInTheDocument()
  })

  it('passes correct props to FileTreeItem components', () => {
    const onFileSelect = vi.fn()
    const selectedFile = mockFiles[0]

    render(<FileTree files={mockFiles} selectedFile={selectedFile} onFileSelect={onFileSelect} />)

    // First file should have selected state
    const firstFileButton = screen.getByText('main.py').closest('button')
    expect(firstFileButton).toHaveClass('bg-accent')

    // Second file should not have selected state
    const secondFileButton = screen.getByText('config.yaml').closest('button')
    expect(secondFileButton).not.toHaveClass('bg-accent')
  })

  it('renders empty state when no files', () => {
    const onFileSelect = vi.fn()

    render(<FileTree files={[]} selectedFile={null} onFileSelect={onFileSelect} />)

    // Should render the ScrollArea but no file items
    expect(screen.queryByText('main.py')).not.toBeInTheDocument()
    expect(screen.queryByText('config.yaml')).not.toBeInTheDocument()
  })
})
