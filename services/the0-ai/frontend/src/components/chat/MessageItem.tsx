import { formatDistanceToNow } from 'date-fns'
import { Bot, User } from 'lucide-react'
import { Message } from '@/types'
import { TypingIndicator } from './TypingIndicator'
import { ArtifactIndicator } from '@/components/common/ArtifactIndicator'
import { useArtifactsStore } from '@/stores/artifactsStore'
import { useQueryClient } from '@tanstack/react-query'
import { StreamingMessage } from './StreamingMessage'
import { Markdown } from '@/components/ui/markdown'

interface MessageItemProps {
  message: Message
  onContentUpdate?: () => void
}

export function MessageItem({ message, onContentUpdate }: MessageItemProps) {
  const isUser = message.role === 'user'
  const isSystem = message.role === 'system'
  const { setForceShow } = useArtifactsStore()
  const queryClient = useQueryClient()

  // Extract mentioned artifacts from assistant messages
  const extractArtifacts = (content: string): string[] => {
    if (isUser || isSystem) return []

    const patterns = [
      /(?:created?|saved?|generated?)\s+(?:file|artifact):\s*([^\s\n]+)/gi,
      /`([^`]+\.(py|yaml|yml|json|md|js|ts|txt))`/gi,
      /\*\*([^*]+\.(py|yaml|yml|json|md|js|ts|txt))\*\*/gi,
    ]

    const artifacts = new Set<string>()

    patterns.forEach(pattern => {
      const matches = [...content.matchAll(pattern)]
      matches.forEach(match => {
        const filename = match[1]
        if (filename && filename.includes('.')) {
          artifacts.add(filename)
        }
      })
    })

    return Array.from(artifacts)
  }

  const artifacts = extractArtifacts(message.content)

  if (isSystem) {
    return (
      <div className="flex justify-center my-4">
        <div className="text-sm text-muted-foreground bg-muted px-3 py-1 rounded-full">
          {message.content}
        </div>
      </div>
    )
  }

  return (
    <div className={`w-full px-4 py-6 ${isUser ? 'bg-background/50' : 'bg-muted/30'}`}>
      <div className="max-w-4xl mx-auto flex gap-4">
        <div className="flex-shrink-0">
          <div
            className={`w-8 h-8 rounded-full flex items-center justify-center ${
              isUser ? 'bg-primary text-primary-foreground' : 'bg-muted'
            }`}
          >
            {isUser ? <User className="w-4 h-4" /> : <Bot className="w-4 h-4" />}
          </div>
        </div>

        <div className="flex-1 min-w-0">
          <div className="text-xs text-muted-foreground mb-3">
            {isUser ? 'You' : 'Assistant'} •{' '}
            {formatDistanceToNow(message.timestamp, { addSuffix: true })}
          </div>

          <div className="prose prose-neutral dark:prose-invert max-w-none">
            {message.isTyping ? (
              <TypingIndicator />
            ) : !isUser && (message.isStreaming === true || message.isComplete === false) ? (
              <StreamingMessage
                content={message.content}
                isStreaming={message.isStreaming}
                isComplete={message.isComplete}
                onContentUpdate={onContentUpdate}
              />
            ) : (
              <Markdown>{message.content}</Markdown>
            )}
          </div>

          {!isUser && artifacts.length > 0 && (
            <div className="mt-4">
              <ArtifactIndicator
                artifacts={artifacts}
                onViewArtifacts={async () => {
                  setForceShow(true)
                  await queryClient.invalidateQueries({ queryKey: ['artifacts'] })
                }}
              />
            </div>
          )}
        </div>
      </div>
    </div>
  )
}
