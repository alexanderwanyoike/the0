import { useEffect } from 'react'
import { useChatStore } from '@/stores/chatStore'
import { chatService } from '@/services/chatService'

export function useChatSessions() {
  const {
    sessions,
    currentSessionId,
    messages,
    setSessions,
    setCurrentSession,
    loadSessionMessages,
    createNewSession,
    setSessionId,
  } = useChatStore()

  // Load sessions and most recent session on mount
  useEffect(() => {
    loadSessionsAndRestore()
  }, [])

  const loadSessionsAndRestore = async () => {
    try {
      // Load all sessions
      await loadSessions()

      // Load the most recent session automatically
      const mostRecentSession = await chatService.getMostRecentSession()
      if (mostRecentSession) {
        await loadSession(mostRecentSession.id)
      }
    } catch (error) {
      console.error('Failed to load sessions and restore:', error)
    }
  }

  const loadSessions = async () => {
    try {
      const sessionsData = await chatService.getSessions()
      setSessions(sessionsData)
    } catch (error) {
      console.error('Failed to load sessions:', error)
    }
  }

  const loadSession = async (sessionId: string) => {
    try {
      const sessionData = await chatService.getSession(sessionId)
      const messages = chatService.convertToMessages(sessionData.messages)

      setCurrentSession(sessionId)
      setSessionId(sessionId)
      loadSessionMessages(messages)
    } catch (error) {
      console.error('Failed to load session:', error)
      // If session fails to load, create a new one
      createNewSession()
    }
  }

  const startNewChat = () => {
    createNewSession()
  }

  const switchToSession = async (sessionId: string) => {
    await loadSession(sessionId)
  }

  // Function to refresh sessions from outside
  const refreshSessions = async () => {
    await loadSessions()
  }

  return {
    sessions,
    currentSessionId,
    messages,
    loadSessions,
    loadSession,
    startNewChat,
    switchToSession,
    refreshSessions,
  }
}
