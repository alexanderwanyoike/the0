import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { exportArtifacts, isRunningInPywebview, getExportSizeInfo } from '../exportService'
import { ArtifactFile } from '@/types'

// Mock JSZip
vi.mock('jszip', () => {
  const mockZip = {
    file: vi.fn(),
    generateAsync: vi
      .fn()
      .mockResolvedValue(new Blob(['mock zip content'], { type: 'application/zip' })),
  }
  return {
    default: vi.fn(() => mockZip),
  }
})

// Mock zipGenerator
vi.mock('@/utils/zipGenerator', () => ({
  generateArtifactZip: vi
    .fn()
    .mockResolvedValue(new Blob(['mock zip'], { type: 'application/zip' })),
  generateExportFilename: vi.fn().mockReturnValue('test-export_2024-01-01_12-00-00.zip'),
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

// Mock DOM methods
const mockCreateElement = vi.fn()
const mockAppendChild = vi.fn()
const mockRemoveChild = vi.fn()
const mockClick = vi.fn()
const mockCreateObjectURL = vi.fn().mockReturnValue('blob:mock-url')
const mockRevokeObjectURL = vi.fn()

beforeEach(async () => {
  // Reset DOM mocks
  Object.defineProperty(document, 'createElement', {
    value: mockCreateElement.mockReturnValue({
      click: mockClick,
      href: '',
      download: '',
    }),
    writable: true,
  })

  Object.defineProperty(document.body, 'appendChild', {
    value: mockAppendChild,
    writable: true,
  })

  Object.defineProperty(document.body, 'removeChild', {
    value: mockRemoveChild,
    writable: true,
  })

  Object.defineProperty(window.URL, 'createObjectURL', {
    value: mockCreateObjectURL,
    writable: true,
  })

  Object.defineProperty(window.URL, 'revokeObjectURL', {
    value: mockRevokeObjectURL,
    writable: true,
  })

  // Reset navigator
  Object.defineProperty(navigator, 'userAgent', {
    value: 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36',
    writable: true,
  })

  // Clear window.pywebview
  delete (window as any).pywebview

  vi.clearAllMocks()

  // Re-setup mocks for each test
  const { generateExportFilename, generateArtifactZip } = await import('@/utils/zipGenerator')
  vi.mocked(generateExportFilename).mockReturnValue('test-export_2024-01-01_12-00-00.zip')

  // Create a proper mock blob with arrayBuffer method for pywebview tests
  const mockBlob = new Blob(['mock zip'], { type: 'application/zip' })
  mockBlob.arrayBuffer = vi.fn().mockResolvedValue(new ArrayBuffer(8))
  vi.mocked(generateArtifactZip).mockResolvedValue(mockBlob)
})

afterEach(() => {
  vi.restoreAllMocks()
})

describe('exportService', () => {
  describe('isRunningInPywebview', () => {
    it('returns false in normal browser environment', () => {
      expect(isRunningInPywebview()).toBe(false)
    })

    it('returns true when pywebview API is available', () => {
      ;(window as any).pywebview = { api: {} }

      expect(isRunningInPywebview()).toBe(true)
    })

    it('returns true when user agent contains pywebview', () => {
      Object.defineProperty(navigator, 'userAgent', {
        value: 'Mozilla/5.0 pywebview/4.0',
        writable: true,
      })

      expect(isRunningInPywebview()).toBe(true)
    })

    it('returns true when history API is not available', () => {
      const originalPushState = window.history.pushState
      const originalOpener = window.opener

      Object.defineProperty(window.history, 'pushState', { value: undefined, writable: true })
      Object.defineProperty(window, 'opener', { value: null, writable: true })

      expect(isRunningInPywebview()).toBe(true)

      // Restore
      Object.defineProperty(window.history, 'pushState', {
        value: originalPushState,
        writable: true,
      })
      Object.defineProperty(window, 'opener', { value: originalOpener, writable: true })
    })
  })

  describe('exportArtifacts', () => {
    it('exports files as ZIP successfully in browser', async () => {
      const result = await exportArtifacts(mockFiles)

      expect(result.success).toBe(true)
      expect(result.filename).toBe('test-export_2024-01-01_12-00-00.zip')
      expect(mockCreateElement).toHaveBeenCalledWith('a')
      expect(mockClick).toHaveBeenCalled()
    })

    it('exports files as JSON when format is specified', async () => {
      const result = await exportArtifacts(mockFiles, { format: 'json' })

      expect(result.success).toBe(true)
      expect(result.filename).toMatch(/\.json$/)
    })

    it('handles empty file list', async () => {
      const result = await exportArtifacts([])

      expect(result.success).toBe(false)
      expect(result.error).toBe('No files to export')
    })

    it('uses custom filename when provided', async () => {
      const customFilename = 'my-custom-export.zip'
      const result = await exportArtifacts(mockFiles, { filename: customFilename })

      expect(result.success).toBe(true)
      expect(result.filename).toBe(customFilename)
    })

    it('handles export errors gracefully', async () => {
      // Mock an error in ZIP generation
      const { generateArtifactZip } = await import('@/utils/zipGenerator')
      vi.mocked(generateArtifactZip).mockRejectedValueOnce(new Error('ZIP generation failed'))

      const result = await exportArtifacts(mockFiles)

      expect(result.success).toBe(false)
      expect(result.error).toBe('ZIP generation failed')
    })
  })

  describe('pywebview integration', () => {
    it('uses pywebview API when available', async () => {
      const mockSaveFile = vi.fn().mockResolvedValue('/saved/path/file.zip')
      ;(window as any).pywebview = {
        api: {
          save_file: mockSaveFile,
        },
      }

      const result = await exportArtifacts(mockFiles)

      expect(result.success).toBe(true)
      expect(mockSaveFile).toHaveBeenCalled()
    })

    it('falls back to browser download when pywebview fails', async () => {
      const mockSaveFile = vi.fn().mockRejectedValue(new Error('Pywebview error'))
      ;(window as any).pywebview = {
        api: {
          save_file: mockSaveFile,
        },
      }

      const result = await exportArtifacts(mockFiles)

      expect(result.success).toBe(true) // Should fall back to browser download
      expect(mockClick).toHaveBeenCalled() // Browser download was used
    })

    it('converts blob to base64 for pywebview', async () => {
      const mockSaveFile = vi.fn().mockResolvedValue('/saved/path/file.zip')
      ;(window as any).pywebview = {
        api: {
          save_file: mockSaveFile,
        },
      }

      await exportArtifacts(mockFiles)

      expect(mockSaveFile).toHaveBeenCalled()
      const [base64Content, filename] = mockSaveFile.mock.calls[0]
      expect(typeof base64Content).toBe('string')
      expect(filename).toMatch(/\.zip$/)
    })
  })

  describe('getExportSizeInfo', () => {
    it('calculates size information correctly', () => {
      const sizeInfo = getExportSizeInfo(mockFiles)

      expect(sizeInfo.fileCount).toBe(2)
      expect(sizeInfo.totalSize).toBeGreaterThan(0)
      expect(sizeInfo.estimatedZipSize).toBeGreaterThan(0)
      // For small files, estimated size includes overhead, so it might be larger
      expect(sizeInfo.estimatedZipSize).toBe(Math.round(sizeInfo.totalSize * 0.4 + 1024))
    })

    it('handles empty file list', () => {
      const sizeInfo = getExportSizeInfo([])

      expect(sizeInfo.fileCount).toBe(0)
      expect(sizeInfo.totalSize).toBe(0)
      expect(sizeInfo.estimatedZipSize).toBe(1024) // Just overhead
    })

    it('ignores folder-type files', () => {
      const filesWithFolder: ArtifactFile[] = [
        ...mockFiles,
        {
          id: 'folder1',
          name: 'utils',
          content: '',
          language: '',
          type: 'folder',
        },
      ]

      const sizeInfo = getExportSizeInfo(filesWithFolder)

      expect(sizeInfo.fileCount).toBe(2) // Still only counts files, not folders
    })
  })
})
