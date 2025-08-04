import { renderHook, act } from '@testing-library/react'
import { describe, it, expect, vi, beforeEach } from 'vitest'
import { useArtifactExport, useExportInfo } from '../useArtifactExport'
import { ArtifactFile } from '@/types'

// Mock the export service
vi.mock('@/services/exportService', () => ({
  exportArtifacts: vi.fn(),
  getExportSizeInfo: vi.fn(),
  isRunningInPywebview: vi.fn(),
}))

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

beforeEach(() => {
  vi.clearAllMocks()
})

describe('useArtifactExport', () => {
  it('initializes with correct default state', () => {
    const { result } = renderHook(() => useArtifactExport())

    expect(result.current.isExporting).toBe(false)
    expect(result.current.lastExportResult).toBe(null)
    expect(typeof result.current.exportFiles).toBe('function')
    expect(typeof result.current.getSizeInfo).toBe('function')
    expect(typeof result.current.canExport).toBe('function')
    expect(typeof result.current.clearLastResult).toBe('function')
  })

  it('correctly identifies when files can be exported', () => {
    const { result } = renderHook(() => useArtifactExport())

    expect(result.current.canExport(mockFiles)).toBe(true)
    expect(result.current.canExport(emptyFiles)).toBe(false)
    expect(result.current.canExport([])).toBe(false)
  })

  it('handles successful export', async () => {
    const { exportArtifacts } = await import('@/services/exportService')
    vi.mocked(exportArtifacts).mockResolvedValueOnce({
      success: true,
      filename: 'test-export.zip',
      size: 1024,
    })

    const { result } = renderHook(() => useArtifactExport())

    let exportResult: any
    await act(async () => {
      exportResult = await result.current.exportFiles(mockFiles)
    })

    expect(exportResult!.success).toBe(true)
    expect(result.current.isExporting).toBe(false)
    expect(result.current.lastExportResult?.success).toBe(true)
    expect(result.current.lastExportResult?.filename).toBe('test-export.zip')
  })

  it('handles export failure', async () => {
    const { exportArtifacts } = await import('@/services/exportService')
    vi.mocked(exportArtifacts).mockResolvedValueOnce({
      success: false,
      error: 'Export failed',
    })

    const { result } = renderHook(() => useArtifactExport())

    let exportResult: any
    await act(async () => {
      exportResult = await result.current.exportFiles(mockFiles)
    })

    expect(exportResult!.success).toBe(false)
    expect(result.current.isExporting).toBe(false)
    expect(result.current.lastExportResult?.success).toBe(false)
    expect(result.current.lastExportResult?.error).toBe('Export failed')
  })

  it('prevents concurrent exports', async () => {
    const { exportArtifacts } = await import('@/services/exportService')
    // Make the first export take some time
    vi.mocked(exportArtifacts).mockImplementation(
      () => new Promise(resolve => setTimeout(() => resolve({ success: true }), 100))
    )

    const { result } = renderHook(() => useArtifactExport())

    let firstResult: any, secondResult: any

    await act(async () => {
      // Start first export
      const firstPromise = result.current.exportFiles(mockFiles)
      expect(result.current.isExporting).toBe(true)

      // Try to start second export while first is running
      secondResult = await result.current.exportFiles(mockFiles)

      // Wait for first to complete
      firstResult = await firstPromise
    })

    expect(firstResult!.success).toBe(true)
    expect(secondResult!.success).toBe(false)
    expect(secondResult!.error).toBe('Export already in progress')
  })

  it('handles export exceptions', async () => {
    const { exportArtifacts } = await import('@/services/exportService')
    vi.mocked(exportArtifacts).mockRejectedValueOnce(new Error('Unexpected error'))

    const { result } = renderHook(() => useArtifactExport())

    let exportResult: any
    await act(async () => {
      exportResult = await result.current.exportFiles(mockFiles)
    })

    expect(exportResult!.success).toBe(false)
    expect(exportResult!.error).toBe('Unexpected error')
    expect(result.current.isExporting).toBe(false)
  })

  it('clears last result', () => {
    const { result } = renderHook(() => useArtifactExport())

    // Set some result first
    act(() => {
      ;(result.current as any).lastExportResult = { success: true }
    })

    act(() => {
      result.current.clearLastResult()
    })

    expect(result.current.lastExportResult).toBe(null)
  })

  it('passes options to export service', async () => {
    const { exportArtifacts } = await import('@/services/exportService')
    vi.mocked(exportArtifacts).mockResolvedValueOnce({ success: true })

    const { result } = renderHook(() => useArtifactExport())

    const options = { filename: 'custom.zip', format: 'zip' as const }

    await act(async () => {
      await result.current.exportFiles(mockFiles, options)
    })

    expect(exportArtifacts).toHaveBeenCalledWith(mockFiles, options)
  })

  it('returns size info from service', async () => {
    const { getExportSizeInfo } = await import('@/services/exportService')
    vi.mocked(getExportSizeInfo).mockReturnValue({
      fileCount: 2,
      totalSize: 1000,
      estimatedZipSize: 400,
    })

    const { result } = renderHook(() => useArtifactExport())

    const sizeInfo = result.current.getSizeInfo(mockFiles)

    expect(sizeInfo.fileCount).toBe(2)
    expect(sizeInfo.totalSize).toBe(1000)
    expect(sizeInfo.estimatedZipSize).toBe(400)
  })
})

describe('useExportInfo', () => {
  beforeEach(async () => {
    const { getExportSizeInfo, isRunningInPywebview } = await import('@/services/exportService')

    vi.mocked(getExportSizeInfo).mockReturnValue({
      fileCount: 2,
      totalSize: 1000,
      estimatedZipSize: 400,
    })

    vi.mocked(isRunningInPywebview).mockReturnValue(false)
  })

  it('returns correct export info for files', () => {
    const { result } = renderHook(() => useExportInfo(mockFiles))

    expect(result.current.fileCount).toBe(2)
    expect(result.current.totalSize).toBe(1000)
    expect(result.current.estimatedZipSize).toBe(400)
    expect(result.current.canExport).toBe(true)
    expect(result.current.hasFiles).toBe(true)
    expect(result.current.isPywebview).toBe(false)
  })

  it('returns correct info for empty files', async () => {
    const { getExportSizeInfo } = await import('@/services/exportService')
    vi.mocked(getExportSizeInfo).mockReturnValue({
      fileCount: 0,
      totalSize: 0,
      estimatedZipSize: 1024,
    })

    const { result } = renderHook(() => useExportInfo([]))

    expect(result.current.fileCount).toBe(0)
    expect(result.current.canExport).toBe(false)
    expect(result.current.hasFiles).toBe(false)
  })

  it('detects pywebview environment', async () => {
    const { isRunningInPywebview } = await import('@/services/exportService')
    vi.mocked(isRunningInPywebview).mockReturnValue(true)

    const { result } = renderHook(() => useExportInfo(mockFiles))

    expect(result.current.isPywebview).toBe(true)
  })
})
