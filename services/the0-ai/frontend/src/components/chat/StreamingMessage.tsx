import { useEffect, useRef } from 'react'
import { useStreamingTypewriter } from '@/hooks/useStreamingTypewriter'
import { useSettingsStore } from '@/stores/settingsStore'
import { Markdown } from '@/components/ui/markdown'

interface StreamingMessageProps {
  content: string
  isStreaming?: boolean
  isComplete?: boolean
  onTypewriterComplete?: () => void
  onContentUpdate?: () => void
}

export function StreamingMessage({
  content,
  isStreaming = false,
  isComplete = false,
  onTypewriterComplete,
  onContentUpdate,
}: StreamingMessageProps) {
  const { streamingEnabled, typewriterEnabled, typewriterSpeed } = useSettingsStore()
  const lastContentRef = useRef('')

  const { displayText, isTyping, addChunk, setCompleteText, complete, reset } =
    useStreamingTypewriter({
      speed: typewriterSpeed,
      enabled: streamingEnabled && typewriterEnabled && isStreaming,
      onUpdate: onContentUpdate,
    })

  // Reset when starting a new message
  useEffect(() => {
    if (isStreaming && content === '') {
      reset()
      lastContentRef.current = ''
    }
  }, [isStreaming, content, reset])

  // When content changes and we're streaming, add the new chunk
  useEffect(() => {
    console.log('🔍 StreamingMessage useEffect:', {
      content: content.slice(0, 100),
      contentLength: content.length,
      lastContentLength: lastContentRef.current.length,
      isStreaming,
      streamingEnabled,
      typewriterEnabled,
      isNewContent: content !== lastContentRef.current,
      startsWithLast: content.startsWith(lastContentRef.current),
    })

    if (
      isStreaming &&
      streamingEnabled &&
      typewriterEnabled &&
      content !== lastContentRef.current
    ) {
      if (content.startsWith(lastContentRef.current)) {
        // New content is an addition to previous content
        const newChunk = content.slice(lastContentRef.current.length)
        if (newChunk) {
          console.log('✅ Adding chunk to typewriter:', {
            chunk: newChunk,
            length: newChunk.length,
          })
          addChunk(newChunk)
          onContentUpdate?.()
        }
      } else {
        // Content replaced completely (shouldn't happen in streaming but handle it)
        console.log('⚠️ Content replaced completely, setting full text')
        setCompleteText(content)
        onContentUpdate?.()
      }
      lastContentRef.current = content
    } else if (!isStreaming || !streamingEnabled || !typewriterEnabled) {
      // Not streaming or typewriter disabled - show content immediately
      console.log('📝 Setting complete text (no streaming/typewriter)')
      setCompleteText(content)
      lastContentRef.current = content
      onContentUpdate?.()
    }
  }, [
    content,
    isStreaming,
    streamingEnabled,
    typewriterEnabled,
    addChunk,
    setCompleteText,
    onContentUpdate,
  ])

  // When streaming completes, finish typewriter
  useEffect(() => {
    if (isComplete && isTyping) {
      complete()
    }
  }, [isComplete, isTyping, complete])

  // Notify parent when typewriter completes
  useEffect(() => {
    if (!isTyping && isComplete && onTypewriterComplete) {
      onTypewriterComplete()
    }
  }, [isTyping, isComplete, onTypewriterComplete])

  const showCursor = isStreaming && (isTyping || !isComplete)
  const textToShow = streamingEnabled && typewriterEnabled && isStreaming ? displayText : content

  return (
    <div className="relative">
      <Markdown>{textToShow}</Markdown>
      {showCursor && (
        <span className="inline-block w-2 h-5 bg-primary animate-pulse ml-1 align-text-bottom" />
      )}
    </div>
  )
}
