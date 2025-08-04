import { useMutation } from '@tanstack/react-query'
import { useChatStore } from '@/stores/chatStore'
import { apiService } from '@/services/api'
import { chatService } from '@/services/chatService'
import { Message } from '@/types'

export const useChat = () => {
  const {
    messages,
    sessionId,
    isTyping,
    addMessage,
    setTyping,
    setSessionId,
    setCurrentSession,
    updateSessionTitle,
    setSessions,
    sessions,
  } = useChatStore()

  const sendMessageMutation = useMutation({
    mutationFn: (request: { message: string; session_id?: string }) =>
      apiService.sendMessage(request),
    onMutate: async request => {
      // Add user message immediately (optimistic update)
      const userMessage: Message = {
        id: Date.now().toString(),
        content: request.message,
        role: 'user',
        timestamp: new Date(),
      }

      addMessage(userMessage)
      setTyping(true)

      return { userMessage }
    },
    onSuccess: async (response, variables) => {
      setTyping(false)

      // Update session ID if received
      if (response.session_id && response.session_id !== sessionId) {
        setSessionId(response.session_id)
        setCurrentSession(response.session_id)

        // Auto-generate title for new sessions
        if (!sessionId) {
          const title = chatService.generateTitleFromMessage(variables.message)

          // Optimistically add the session to the store first
          const newSession = {
            id: response.session_id,
            title: title,
            created_at: new Date().toISOString(),
            updated_at: new Date().toISOString(),
            is_active: true,
          }

          // Add to sessions list immediately
          setSessions([newSession, ...sessions])

          try {
            await chatService.updateSessionTitle(response.session_id, title)
            // Update the session in the store with the server response
            updateSessionTitle(response.session_id, title)
          } catch (error) {
            console.error('Failed to update session title:', error)
            // On error, refresh from server to get the correct state
            try {
              const updatedSessions = await chatService.getSessions()
              setSessions(updatedSessions)
            } catch (refreshError) {
              console.error('Failed to refresh sessions:', refreshError)
            }
          }
        }
      }

      // Add AI response
      const aiMessage: Message = {
        id: Date.now().toString(),
        content: response.response,
        role: 'assistant',
        timestamp: new Date(),
        artifacts:
          response.artifacts && response.artifacts.length > 0 ? response.artifacts : undefined,
      }

      addMessage(aiMessage)
    },
    onError: error => {
      setTyping(false)

      // Add error message
      const errorMessage: Message = {
        id: Date.now().toString(),
        content: `Error: ${error.message}`,
        role: 'system',
        timestamp: new Date(),
      }

      addMessage(errorMessage)
    },
  })

  const sendMessage = (content: string) => {
    sendMessageMutation.mutate({
      message: content,
      session_id: sessionId || undefined,
    })
  }

  return {
    messages,
    isTyping: isTyping || sendMessageMutation.isPending,
    sendMessage,
    isLoading: sendMessageMutation.isPending,
    error: sendMessageMutation.error,
  }
}
