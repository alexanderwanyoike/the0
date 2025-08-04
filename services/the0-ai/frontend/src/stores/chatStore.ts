import { create } from 'zustand'
import { Message } from '@/types'

interface ChatSession {
  id: string
  title: string
  created_at: string
  updated_at: string
  is_active: boolean
}

interface ChatStore {
  // Current session data
  messages: Message[]
  sessionId: string | null
  isTyping: boolean

  // Session management
  sessions: ChatSession[]
  currentSessionId: string | null

  // Message actions
  addMessage: (message: Message) => void
  updateMessage: (messageId: string, updates: Partial<Message>) => void
  appendToMessage: (messageId: string, content: string) => void
  setTyping: (isTyping: boolean) => void
  clearMessages: () => void
  setSessionId: (sessionId: string) => void

  // Session actions
  setSessions: (sessions: ChatSession[]) => void
  setCurrentSession: (sessionId: string) => void
  createNewSession: () => void
  loadSessionMessages: (messages: Message[]) => void
  updateSessionTitle: (sessionId: string, title: string) => void
  removeSession: (sessionId: string) => void
}

const getDefaultWelcomeMessage = (): Message => ({
  id: '1',
  content:
    'Good day, sir. I am the0, your personal AI assistant for building automated trading bots. How may I assist you in creating your trading strategy today?',
  role: 'assistant',
  timestamp: new Date(),
  isStreaming: false,
  isComplete: true,
})

export const useChatStore = create<ChatStore>(set => ({
  // Current session state
  messages: [getDefaultWelcomeMessage()],
  sessionId: null,
  isTyping: false,

  // Session management state
  sessions: [],
  currentSessionId: null,

  // Message actions
  addMessage: message =>
    set(state => ({
      messages: [...state.messages, message],
    })),

  updateMessage: (messageId, updates) =>
    set(state => ({
      messages: state.messages.map(msg => (msg.id === messageId ? { ...msg, ...updates } : msg)),
    })),

  appendToMessage: (messageId, content) =>
    set(state => ({
      messages: state.messages.map(msg =>
        msg.id === messageId ? { ...msg, content: msg.content + content } : msg
      ),
    })),

  setTyping: isTyping => set(() => ({ isTyping })),

  clearMessages: () =>
    set(() => ({
      messages: [getDefaultWelcomeMessage()],
    })),

  setSessionId: sessionId => set(() => ({ sessionId })),

  // Session actions
  setSessions: sessions => set(() => ({ sessions })),

  setCurrentSession: sessionId =>
    set(() => ({
      currentSessionId: sessionId,
      sessionId: sessionId,
    })),

  createNewSession: () =>
    set(() => ({
      messages: [getDefaultWelcomeMessage()],
      sessionId: null,
      currentSessionId: null,
      isTyping: false,
    })),

  loadSessionMessages: messages => set(() => ({ messages })),

  updateSessionTitle: (sessionId, title) =>
    set(state => ({
      sessions: state.sessions.map(session =>
        session.id === sessionId ? { ...session, title } : session
      ),
    })),

  removeSession: sessionId =>
    set(state => ({
      sessions: state.sessions.filter(session => session.id !== sessionId),
      // If we're removing the current session, reset to new session
      ...(state.currentSessionId === sessionId
        ? {
            messages: [getDefaultWelcomeMessage()],
            sessionId: null,
            currentSessionId: null,
            isTyping: false,
          }
        : {}),
    })),
}))
