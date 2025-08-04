import { render, screen, fireEvent, waitFor } from '@testing-library/react'
import { describe, it, expect, vi, beforeEach } from 'vitest'
import { ArtifactToolbar } from '../ArtifactToolbar'
import { ArtifactFile } from '@/types'

// Mock the export hook
vi.mock('@/hooks/useArtifactExport')

const mockFiles: ArtifactFile[] = [
  {
    id: 'file1',
    name: 'strategy.py',
    content: 'def momentum_strategy():\n    return "buy_signal"',
    language: 'python',
    type: 'file',
  },
  {
    id: 'file2',
    name: 'config.yaml',
    content: 'strategy:\n  name: momentum',
    language: 'yaml',
    type: 'file',
  },
]

const emptyFiles: ArtifactFile[] = []

const mockExportHook = {
  isExporting: false,
  lastExportResult: null as any,
  exportFiles: vi.fn(),
  canExport: vi.fn(),
  clearLastResult: vi.fn(),
  getSizeInfo: vi.fn(),
  isPywebview: false,
}

beforeEach(async () => {
  vi.clearAllMocks()

  // Reset mock implementations
  mockExportHook.exportFiles.mockClear().mockResolvedValue({ success: true })
  mockExportHook.canExport.mockClear().mockReturnValue(true)
  mockExportHook.clearLastResult.mockClear()
  mockExportHook.isExporting = false
  mockExportHook.lastExportResult = null

  // Set up the mock hook to return our mock object
  const { useArtifactExport } = await import('@/hooks/useArtifactExport')
  vi.mocked(useArtifactExport).mockReturnValue(mockExportHook)
})

describe('ArtifactToolbar', () => {
  it('renders export and close buttons', () => {
    const onClose = vi.fn()

    render(<ArtifactToolbar files={mockFiles} onClose={onClose} />)

    expect(screen.getByTitle('Export all files as ZIP')).toBeInTheDocument()
    expect(screen.getByTitle('Close artifacts panel')).toBeInTheDocument()
  })

  it('calls onClose when close button is clicked', () => {
    const onClose = vi.fn()

    render(<ArtifactToolbar files={mockFiles} onClose={onClose} />)

    fireEvent.click(screen.getByTitle('Close artifacts panel'))

    expect(onClose).toHaveBeenCalledOnce()
  })

  it('calls export when export button is clicked', async () => {
    const onClose = vi.fn()

    render(<ArtifactToolbar files={mockFiles} onClose={onClose} />)

    fireEvent.click(screen.getByTitle('Export all files as ZIP'))

    await waitFor(() => {
      expect(mockExportHook.exportFiles).toHaveBeenCalledWith(mockFiles)
    })
  })

  it('disables export button when no files can be exported', () => {
    mockExportHook.canExport.mockReturnValue(false)
    const onClose = vi.fn()

    render(<ArtifactToolbar files={emptyFiles} onClose={onClose} />)

    const exportButton = screen.getByTitle('No files to export')
    expect(exportButton).toBeDisabled()
  })

  it('shows loading state during export', () => {
    mockExportHook.isExporting = true
    const onClose = vi.fn()

    render(<ArtifactToolbar files={mockFiles} onClose={onClose} />)

    expect(screen.getByTitle('Exporting...')).toBeInTheDocument()
    expect(screen.getByTitle('Exporting...')).toBeDisabled()
    expect(screen.getByTestId('loader-icon')).toBeInTheDocument()
  })

  it('shows success state after successful export', () => {
    mockExportHook.lastExportResult = { success: true, filename: 'test.zip' } as any
    const onClose = vi.fn()

    render(<ArtifactToolbar files={mockFiles} onClose={onClose} />)

    const exportButton = screen.getByTitle('Export all files as ZIP')
    expect(exportButton).toHaveClass('text-green-600')
  })

  it('shows error state after failed export', () => {
    mockExportHook.lastExportResult = { success: false, error: 'Export failed' } as any
    const onClose = vi.fn()

    render(<ArtifactToolbar files={mockFiles} onClose={onClose} />)

    const exportButton = screen.getByTitle('Export all files as ZIP')
    expect(exportButton).toHaveClass('text-red-600')
  })

  it('clears result before new export', async () => {
    const onClose = vi.fn()

    render(<ArtifactToolbar files={mockFiles} onClose={onClose} />)

    fireEvent.click(screen.getByTitle('Export all files as ZIP'))

    await waitFor(() => {
      expect(mockExportHook.clearLastResult).toHaveBeenCalled()
    })
  })

  it('handles export errors gracefully', async () => {
    mockExportHook.exportFiles.mockResolvedValue({
      success: false,
      error: 'Export failed',
    })

    const consoleSpy = vi.spyOn(console, 'error').mockImplementation(() => {})
    const onClose = vi.fn()

    render(<ArtifactToolbar files={mockFiles} onClose={onClose} />)

    fireEvent.click(screen.getByTitle('Export all files as ZIP'))

    await waitFor(() => {
      expect(consoleSpy).toHaveBeenCalledWith('Export failed:', 'Export failed')
    })

    consoleSpy.mockRestore()
  })

  it('prevents export when already exporting', async () => {
    mockExportHook.isExporting = true
    const onClose = vi.fn()

    render(<ArtifactToolbar files={mockFiles} onClose={onClose} />)

    fireEvent.click(screen.getByTitle('Exporting...'))

    // Should not call export since it's disabled
    expect(mockExportHook.exportFiles).not.toHaveBeenCalled()
  })

  it('applies custom className', () => {
    const onClose = vi.fn()

    render(<ArtifactToolbar files={mockFiles} onClose={onClose} className="custom-class" />)

    const toolbar = screen.getByTitle('Export all files as ZIP').closest('div')
    expect(toolbar).toHaveClass('custom-class')
  })

  it('handles successful export with console log', async () => {
    mockExportHook.exportFiles.mockResolvedValue({
      success: true,
      filename: 'test-export.zip',
    })

    const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {})
    const onClose = vi.fn()

    render(<ArtifactToolbar files={mockFiles} onClose={onClose} />)

    fireEvent.click(screen.getByTitle('Export all files as ZIP'))

    await waitFor(() => {
      expect(consoleSpy).toHaveBeenCalledWith('Export successful:', 'test-export.zip')
    })

    consoleSpy.mockRestore()
  })

  describe('accessibility', () => {
    it('has proper ARIA labels', () => {
      const onClose = vi.fn()

      render(<ArtifactToolbar files={mockFiles} onClose={onClose} />)

      expect(screen.getByText('Export artifacts')).toBeInTheDocument()
      expect(screen.getByText('Close artifacts panel')).toBeInTheDocument()
    })

    it('has proper button titles for tooltips', () => {
      const onClose = vi.fn()

      render(<ArtifactToolbar files={mockFiles} onClose={onClose} />)

      expect(screen.getByTitle('Export all files as ZIP')).toBeInTheDocument()
      expect(screen.getByTitle('Close artifacts panel')).toBeInTheDocument()
    })

    it('shows appropriate titles for different states', () => {
      mockExportHook.canExport.mockReturnValue(false)
      const onClose = vi.fn()

      render(<ArtifactToolbar files={emptyFiles} onClose={onClose} />)

      expect(screen.getByTitle('No files to export')).toBeInTheDocument()
    })
  })
})
