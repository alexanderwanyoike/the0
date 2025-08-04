import { useState, useCallback } from 'react'
import { useChatStore } from '@/stores/chatStore'
import { apiService } from '@/services/api'
import { Message, ChatRequest } from '@/types'
import { v4 as uuidv4 } from 'uuid'

// Simulated streaming for when backend isn't available
export function useSimulatedStreaming() {
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

          // Try real streaming first, fall back to simulated
          try {
            await apiService.sendMessageStream(request, chunk => {
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
                  throw new Error(chunk.error || 'Streaming error')
              }
            })
          } catch (streamingError) {
            console.log('Real streaming failed, using simulated streaming:', streamingError)

            // Fallback to simulated streaming with real API response
            const response = await apiService.sendMessage(request)

            // Simulate streaming by breaking response into chunks
            const chunks = response.response.match(/.{1,5}/g) || [response.response]

            // Reset message content for simulation
            updateMessage(assistantMessageId, { content: '', isStreaming: true, isComplete: false })

            for (let i = 0; i < chunks.length; i++) {
              await new Promise(resolve => setTimeout(resolve, 100)) // 100ms delay between chunks
              appendToMessage(assistantMessageId, chunks[i])
            }

            // Complete the streaming simulation
            updateMessage(assistantMessageId, {
              isStreaming: false,
              isComplete: true,
              artifacts: response.artifacts,
            })

            if (response.session_id && !sessionId) {
              setSessionId(response.session_id)
            }
          }
        } else {
          // Regular non-streaming
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
