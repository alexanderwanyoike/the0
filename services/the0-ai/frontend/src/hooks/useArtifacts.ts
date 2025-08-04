import { useEffect } from 'react'
import { useQuery } from '@tanstack/react-query'
import { useArtifactsStore } from '@/stores/artifactsStore'
import { useChatStore } from '@/stores/chatStore'
import { apiService } from '@/services/api'
import { ArtifactFile } from '@/types'

const getLanguageFromName = (fileName: string) => {
  const ext = fileName.split('.').pop()?.toLowerCase()
  switch (ext) {
    case 'py':
      return 'python'
    case 'yaml':
    case 'yml':
      return 'yaml'
    case 'json':
      return 'json'
    case 'md':
      return 'markdown'
    case 'js':
    case 'jsx':
      return 'javascript'
    case 'ts':
    case 'tsx':
      return 'typescript'
    default:
      return 'plaintext'
  }
}

const createArtifactFile = (name: string): ArtifactFile => ({
  id: name, // Use filename as ID
  name,
  content: '// Failed to load content',
  language: getLanguageFromName(name),
  type: 'file',
})

export const useArtifacts = () => {
  const { forceShow, setFiles, files: storeFiles, setActiveFile } = useArtifactsStore()
  const { sessionId } = useChatStore()

  const {
    data: artifactNames = [],
    isLoading,
    error,
  } = useQuery<string[]>({
    queryKey: ['artifacts', sessionId], // Include sessionId in query key
    queryFn: async () => {
      if (!sessionId) {
        return []
      }
      const result = await apiService.getSessionArtifacts(sessionId)
      return result
    },
    enabled: forceShow && !!sessionId, // Only fetch when artifacts panel is shown AND we have a session
    refetchInterval: false, // Disable automatic refetching to prevent race conditions
  })

  // Fetch content for each file when artifact names change
  useEffect(() => {
    if (artifactNames.length > 0) {
      const fetchFileContents = async () => {
        const filesWithContent = await Promise.all(
          artifactNames.map(async (name: string) => {
            try {
              const artifact = await apiService.getArtifact(name, sessionId || undefined)

              // Handle escaped characters in content
              let content = artifact.content
              if (typeof content === 'string') {
                // Check if content appears to be escaped (has \n but no actual newlines)
                const hasEscapedNewlines = content.includes('\\n') && !content.includes('\n')
                if (hasEscapedNewlines) {
                  content = content
                    .replace(/\\n/g, '\n')
                    .replace(/\\t/g, '\t')
                    .replace(/\\"/g, '"')
                    .replace(/\\'/g, "'")
                    .replace(/\\\\/g, '\\') // Handle escaped backslashes last
                }
              }

              const fileObj = {
                id: name, // Use filename as ID instead of index
                name,
                content,
                language: getLanguageFromName(name),
                type: 'file' as const,
              }

              return fileObj
            } catch (error) {
              console.error(`Failed to fetch content for ${name}:`, error)
              return createArtifactFile(name) // Fallback to placeholder
            }
          })
        )

        // Update files while preserving active file selection
        setFiles(filesWithContent)
      }

      fetchFileContents()
    }
  }, [artifactNames, setFiles, setActiveFile])

  return {
    files: storeFiles, // Use files from store instead of computed files
    isLoading,
    error,
    artifactNames,
  }
}
