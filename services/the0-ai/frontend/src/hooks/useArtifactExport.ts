import { useState } from 'react'
import { ArtifactFile } from '@/types'
import {
  exportArtifacts,
  ExportOptions,
  ExportResult,
  getExportSizeInfo,
  isRunningInPywebview,
} from '@/services/exportService'

export interface UseArtifactExportReturn {
  isExporting: boolean
  lastExportResult: ExportResult | null
  exportFiles: (files: ArtifactFile[], options?: ExportOptions) => Promise<ExportResult>
  getSizeInfo: (files: ArtifactFile[]) => ReturnType<typeof getExportSizeInfo>
  isPywebview: boolean
  canExport: (files: ArtifactFile[]) => boolean
  clearLastResult: () => void
}

/**
 * Hook for managing artifact exports with pywebview support
 */
export function useArtifactExport(): UseArtifactExportReturn {
  const [isExporting, setIsExporting] = useState(false)
  const [lastExportResult, setLastExportResult] = useState<ExportResult | null>(null)

  const exportFiles = async (
    files: ArtifactFile[],
    options: ExportOptions = {}
  ): Promise<ExportResult> => {
    if (isExporting) {
      return {
        success: false,
        error: 'Export already in progress',
      }
    }

    setIsExporting(true)

    try {
      const result = await exportArtifacts(files, options)
      setLastExportResult(result)
      return result
    } catch (error) {
      const errorResult: ExportResult = {
        success: false,
        error: error instanceof Error ? error.message : 'Export failed',
      }
      setLastExportResult(errorResult)
      return errorResult
    } finally {
      setIsExporting(false)
    }
  }

  const getSizeInfo = (files: ArtifactFile[]) => {
    return getExportSizeInfo(files)
  }

  const canExport = (files: ArtifactFile[]): boolean => {
    return files.filter(f => f.type === 'file').length > 0
  }

  const clearLastResult = () => {
    setLastExportResult(null)
  }

  return {
    isExporting,
    lastExportResult,
    exportFiles,
    getSizeInfo,
    isPywebview: isRunningInPywebview(),
    canExport,
    clearLastResult,
  }
}

/**
 * Hook for getting export status and info without triggering exports
 */
export function useExportInfo(files: ArtifactFile[]) {
  const sizeInfo = getExportSizeInfo(files)
  const canExport = files.filter(f => f.type === 'file').length > 0
  const isPywebview = isRunningInPywebview()

  return {
    ...sizeInfo,
    canExport,
    isPywebview,
    hasFiles: files.length > 0,
  }
}
