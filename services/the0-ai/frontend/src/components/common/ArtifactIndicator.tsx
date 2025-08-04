import { FileText, Download } from 'lucide-react'
import { Button } from '@/components/ui/button'

interface ArtifactIndicatorProps {
  artifacts: string[]
  onViewArtifacts?: () => void
}

export function ArtifactIndicator({ artifacts, onViewArtifacts }: ArtifactIndicatorProps) {
  if (artifacts.length === 0) return null

  return (
    <div className="mt-3 p-3 bg-muted/50 rounded-lg border border-border">
      <div className="flex items-center gap-2 mb-2">
        <FileText className="h-4 w-4 text-primary" />
        <span className="text-sm font-medium">
          {artifacts.length} file{artifacts.length !== 1 ? 's' : ''} generated
        </span>
      </div>

      <div className="flex flex-wrap gap-1 mb-3">
        {artifacts.map((artifact, index) => (
          <span key={index} className="text-xs bg-background px-2 py-1 rounded border">
            {artifact}
          </span>
        ))}
      </div>

      {onViewArtifacts && (
        <Button variant="outline" size="sm" onClick={onViewArtifacts} className="h-7 text-xs">
          <Download className="h-3 w-3 mr-1" />
          View Files
        </Button>
      )}
    </div>
  )
}
