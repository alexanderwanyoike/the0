import { Button } from '@/components/ui/button'
import { X, Download, Loader2 } from 'lucide-react'
import { ArtifactFile } from '@/types'
import { useArtifactExport } from '@/hooks/useArtifactExport'
import { useEffect } from 'react'

interface ArtifactToolbarProps {
  files: ArtifactFile[]
  onClose: () => void
  className?: string
}

export function ArtifactToolbar({ files, onClose, className }: ArtifactToolbarProps) {
  const { isExporting, lastExportResult, exportFiles, canExport, clearLastResult } =
    useArtifactExport()

  const handleExport = async () => {
    if (!canExport(files) || isExporting) return

    clearLastResult()
    const result = await exportFiles(files)

    if (result.success) {
      // Success feedback could be handled by a toast system
      console.log('Export successful:', result.filename)
    } else {
      // Error feedback could be handled by a toast system
      console.error('Export failed:', result.error)
    }
  }

  // Clear results after a delay for better UX
  useEffect(() => {
    if (lastExportResult) {
      const timer = setTimeout(clearLastResult, 5000)
      return () => clearTimeout(timer)
    }
  }, [lastExportResult, clearLastResult])

  const exportButtonTitle = (() => {
    if (!canExport(files)) return 'No files to export'
    if (isExporting) return 'Exporting...'
    return 'Export all files as ZIP'
  })()

  const exportButtonClass = (() => {
    if (lastExportResult?.success) return 'text-green-600 hover:text-green-700'
    if (lastExportResult?.error) return 'text-red-600 hover:text-red-700'
    return ''
  })()

  return (
    <div className={`flex gap-1 ${className || ''}`}>
      <Button
        variant="ghost"
        size="icon"
        onClick={handleExport}
        disabled={!canExport(files) || isExporting}
        className={`h-8 w-8 ${exportButtonClass}`}
        title={exportButtonTitle}
      >
        {isExporting ? (
          <Loader2 className="h-4 w-4 animate-spin" data-testid="loader-icon" />
        ) : (
          <Download className="h-4 w-4" />
        )}
        <span className="sr-only">Export artifacts</span>
      </Button>

      <Button
        variant="ghost"
        size="icon"
        onClick={onClose}
        className="h-8 w-8"
        title="Close artifacts panel"
      >
        <X className="h-4 w-4" />
        <span className="sr-only">Close artifacts panel</span>
      </Button>
    </div>
  )
}
