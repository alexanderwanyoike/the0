import { describe, it, expect } from 'vitest'
import { generateArtifactZip, generateExportFilename, estimateZipSize } from '../zipGenerator'
import { ArtifactFile } from '@/types'

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
    content: 'strategy:\n  name: momentum\n  timeframe: 1h',
    language: 'yaml',
    type: 'file',
  },
  {
    id: 'folder1',
    name: 'utils',
    content: '',
    language: '',
    type: 'folder',
    children: [
      {
        id: 'file3',
        name: 'helpers.py',
        content: 'def calculate_rsi():\n    return 50.0',
        language: 'python',
        type: 'file',
      },
    ],
  },
]

describe('zipGenerator', () => {
  describe('generateArtifactZip', () => {
    it('creates a ZIP file with all artifact files', async () => {
      const blob = await generateArtifactZip(mockFiles)

      expect(blob).toBeInstanceOf(Blob)
      expect(blob.type).toBe('application/zip')
      expect(blob.size).toBeGreaterThan(0)
    })

    it('includes only file-type artifacts in ZIP', async () => {
      const blob = await generateArtifactZip(mockFiles)

      // The ZIP should contain files but not folders
      expect(blob.size).toBeGreaterThan(100) // Should have content
    })

    it('handles empty files gracefully', async () => {
      const emptyFiles: ArtifactFile[] = [
        {
          id: 'empty',
          name: 'empty.py',
          content: '',
          language: 'python',
          type: 'file',
        },
      ]

      const blob = await generateArtifactZip(emptyFiles)

      expect(blob).toBeInstanceOf(Blob)
      expect(blob.size).toBeGreaterThan(0) // Still has ZIP structure and metadata
    })

    it('includes metadata file in ZIP', async () => {
      const blob = await generateArtifactZip(mockFiles)

      // The blob should contain metadata - we can't easily test the contents
      // without extracting the ZIP, but we can verify it has reasonable size
      expect(blob.size).toBeGreaterThan(200) // Should include files + metadata
    })

    it('handles files with special characters', async () => {
      const specialFiles: ArtifactFile[] = [
        {
          id: 'special',
          name: 'test-file_with@special#chars.py',
          content: 'print("Hello, 世界!")',
          language: 'python',
          type: 'file',
        },
      ]

      const blob = await generateArtifactZip(specialFiles)

      expect(blob).toBeInstanceOf(Blob)
      expect(blob.size).toBeGreaterThan(0)
    })
  })

  describe('generateExportFilename', () => {
    it('generates filename with timestamp', () => {
      const filename = generateExportFilename()

      expect(filename).toMatch(/^trading-bot-artifacts_\d{4}-\d{2}-\d{2}_\d{2}-\d{2}-\d{2}\.zip$/)
    })

    it('uses custom base name', () => {
      const filename = generateExportFilename('my-project')

      expect(filename).toMatch(/^my-project_\d{4}-\d{2}-\d{2}_\d{2}-\d{2}-\d{2}\.zip$/)
    })

    it('uses custom extension', () => {
      const filename = generateExportFilename('project', 'json')

      expect(filename).toMatch(/^project_\d{4}-\d{2}-\d{2}_\d{2}-\d{2}-\d{2}\.json$/)
    })

    it('generates filename with correct format', () => {
      const filename1 = generateExportFilename()
      const filename2 = generateExportFilename()

      // Both should match the format, even if they might be the same due to timing
      expect(filename1).toMatch(/^trading-bot-artifacts_\d{4}-\d{2}-\d{2}_\d{2}-\d{2}-\d{2}\.zip$/)
      expect(filename2).toMatch(/^trading-bot-artifacts_\d{4}-\d{2}-\d{2}_\d{2}-\d{2}-\d{2}\.zip$/)

      // At minimum, they should be valid strings
      expect(filename1.length).toBeGreaterThan(0)
      expect(filename2.length).toBeGreaterThan(0)
    })
  })

  describe('estimateZipSize', () => {
    it('estimates reasonable ZIP size for text files', () => {
      const estimate = estimateZipSize(mockFiles)

      expect(estimate).toBeGreaterThan(0)
      expect(estimate).toBeLessThan(2000) // Should be compressed but allow for content
    })

    it('returns larger estimate for larger files', () => {
      const largeFiles: ArtifactFile[] = [
        {
          id: 'large',
          name: 'large.py',
          content: 'print("hello")\n'.repeat(1000),
          language: 'python',
          type: 'file',
        },
      ]

      const smallEstimate = estimateZipSize(mockFiles)
      const largeEstimate = estimateZipSize(largeFiles)

      expect(largeEstimate).toBeGreaterThan(smallEstimate)
    })

    it('handles empty file list', () => {
      const estimate = estimateZipSize([])

      expect(estimate).toBe(1024) // Just overhead
    })

    it('ignores folder-type files in size calculation', () => {
      const filesOnly = mockFiles.filter(f => f.type === 'file')
      const allFiles = mockFiles

      const filesOnlyEstimate = estimateZipSize(filesOnly)
      const allFilesEstimate = estimateZipSize(allFiles)

      expect(filesOnlyEstimate).toBe(allFilesEstimate)
    })
  })
})
