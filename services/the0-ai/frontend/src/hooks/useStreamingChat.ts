import { useState, useCallback } from 'react'
import { useChatStore } from '@/stores/chatStore'
import { apiService } from '@/services/api'
import { Message, ChatRequest, StreamChunk } from '@/types'
import { v4 as uuidv4 } from 'uuid'

export function useStreamingChat() {
  const { messages, sessionId, addMessage, updateMessage, appendToMessage, setSessionId } =
    useChatStore()

  const [isStreaming, setIsStreaming] = useState(false)
  const [streamingMessageId, setStreamingMessageId] = useState<string | null>(null)

  const sendMessage = useCallback(
    async (content: string, useStreaming = true) => {
      if (isStreaming) return

      // Add user message
      const userMessage: Message = {
        id: uuidv4(),
        content,
        role: 'user',
        timestamp: new Date(),
        isStreaming: false,
        isComplete: true,
      }
      addMessage(userMessage)

      // Create placeholder assistant message for streaming
      const assistantMessageId = uuidv4()
      const assistantMessage: Message = {
        id: assistantMessageId,
        content: '',
        role: 'assistant',
        timestamp: new Date(),
        isStreaming: useStreaming,
        isComplete: false,
      }
      addMessage(assistantMessage)

      const request: ChatRequest = {
        message: content,
        session_id: sessionId || undefined,
      }

      try {
        if (useStreaming) {
          setIsStreaming(true)
          setStreamingMessageId(assistantMessageId)

          await apiService.sendMessageStream(request, (chunk: StreamChunk) => {
            switch (chunk.type) {
              case 'content':
                if (chunk.content) {
                  appendToMessage(assistantMessageId, chunk.content)
                }
                break

              case 'artifacts':
                if (chunk.artifacts) {
                  updateMessage(assistantMessageId, { artifacts: chunk.artifacts })
                }
                break

              case 'complete':
                updateMessage(assistantMessageId, {
                  isStreaming: false,
                  isComplete: true,
                })
                if (chunk.session_id && !sessionId) {
                  setSessionId(chunk.session_id)
                }
                break

              case 'error':
                updateMessage(assistantMessageId, {
                  content: `Error: ${chunk.error}`,
                  isStreaming: false,
                  isComplete: true,
                })
                break
            }
          })
        } else {
          // Fallback to non-streaming
          const response = await apiService.sendMessage(request)
          updateMessage(assistantMessageId, {
            content: response.response,
            artifacts: response.artifacts,
            isStreaming: false,
            isComplete: true,
          })

          if (response.session_id && !sessionId) {
            setSessionId(response.session_id)
          }
        }
      } catch (error) {
        updateMessage(assistantMessageId, {
          content: `Error: ${error instanceof Error ? error.message : 'Unknown error occurred'}`,
          isStreaming: false,
          isComplete: true,
        })
      } finally {
        setIsStreaming(false)
        setStreamingMessageId(null)
      }
    },
    [isStreaming, sessionId, addMessage, updateMessage, appendToMessage, setSessionId]
  )

  return {
    messages,
    sendMessage,
    isStreaming,
    streamingMessageId,
  }
}
