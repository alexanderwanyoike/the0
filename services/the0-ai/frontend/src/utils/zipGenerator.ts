import JSZip from 'jszip'
import { ArtifactFile } from '@/types'

/**
 * Generates a ZIP file containing all artifact files
 * @param files Array of artifact files to include in the ZIP
 * @param filename Base filename for the ZIP (without extension)
 * @returns Promise that resolves to a Blob containing the ZIP file
 */
export async function generateArtifactZip(
  files: ArtifactFile[],
  _filename: string = 'trading-bot-artifacts'
): Promise<Blob> {
  const zip = new JSZip()

  // Add each file to the ZIP
  for (const file of files) {
    if (file.type === 'file') {
      // Ensure file has content
      const content = file.content || '// Empty file'

      // Add file to ZIP with proper encoding
      zip.file(file.name, content, {
        date: new Date(),
        comment: `Generated artifact: ${file.name}`,
      })
    }
  }

  // Add a metadata file with information about the export
  const metadata = {
    exportDate: new Date().toISOString(),
    fileCount: files.filter(f => f.type === 'file').length,
    generator: 'the0-ai-frontend',
    files: files
      .filter(f => f.type === 'file')
      .map(f => ({
        name: f.name,
        language: f.language,
        size: f.content.length,
      })),
  }

  zip.file('export-metadata.json', JSON.stringify(metadata, null, 2))

  // Generate the ZIP file
  const blob = await zip.generateAsync({
    type: 'blob',
    compression: 'DEFLATE',
    compressionOptions: {
      level: 6,
    },
  })

  return blob
}

/**
 * Generates a timestamped filename for exports
 * @param baseName Base name for the file
 * @param extension File extension (defaults to 'zip')
 * @returns Timestamped filename
 */
export function generateExportFilename(
  baseName: string = 'trading-bot-artifacts',
  extension: string = 'zip'
): string {
  const timestamp = new Date().toISOString().replace(/[:.]/g, '-').replace('T', '_').slice(0, -5) // Remove milliseconds and 'Z'

  return `${baseName}_${timestamp}.${extension}`
}

/**
 * Estimates the size of the ZIP file that would be generated
 * @param files Array of artifact files
 * @returns Estimated size in bytes
 */
export function estimateZipSize(files: ArtifactFile[]): number {
  const contentSize = files
    .filter(f => f.type === 'file')
    .reduce((total, file) => total + (file.content?.length || 0), 0)

  // Rough estimation: ZIP compression typically achieves 60-70% compression for text
  // Add some overhead for ZIP structure and metadata
  return Math.round(contentSize * 0.4 + 1024) // 40% of original size + 1KB overhead
}
