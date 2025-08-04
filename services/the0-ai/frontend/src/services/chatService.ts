import { apiService } from './api'
import { Message } from '@/types'

export interface ChatSession {
  id: string
  title: string
  created_at: string
  updated_at: string
  is_active: boolean
}

export interface ChatMessage {
  id: number
  role: string
  content: string
  artifacts_created: string[] | null
  timestamp: string
}

export interface SessionWithMessages {
  session: ChatSession
  messages: ChatMessage[]
}

class ChatService {
  /**
   * Get all chat sessions for the current user
   */
  async getSessions(): Promise<ChatSession[]> {
    return apiService.getChatSessions()
  }

  /**
   * Get a specific chat session with all its messages
   */
  async getSession(sessionId: string): Promise<SessionWithMessages> {
    return apiService.getChatSession(sessionId)
  }

  /**
   * Update the title of a chat session
   */
  async updateSessionTitle(sessionId: string, title: string): Promise<void> {
    return apiService.updateChatSessionTitle(sessionId, title)
  }

  /**
   * Delete a chat session
   */
  async deleteSession(sessionId: string): Promise<void> {
    return apiService.deleteChatSession(sessionId)
  }

  /**
   * Convert backend chat messages to frontend Message format
   */
  convertToMessages(backendMessages: ChatMessage[]): Message[] {
    return backendMessages.map(msg => ({
      id: msg.id.toString(),
      content: msg.content,
      role: msg.role as 'user' | 'assistant' | 'system',
      timestamp: new Date(msg.timestamp),
      artifacts: msg.artifacts_created || undefined,
      isStreaming: false,
      isComplete: true,
    }))
  }

  /**
   * Get the most recent active session from the database
   */
  async getMostRecentSession(): Promise<ChatSession | null> {
    try {
      const sessions = await this.getSessions()
      return sessions.length > 0 ? sessions[0] : null // Sessions are ordered by updated_at DESC
    } catch (error) {
      console.error('Failed to get most recent session:', error)
      return null
    }
  }

  /**
   * Generate a title from the first user message in a conversation
   */
  generateTitleFromMessage(message: string): string {
    // Take first 50 characters and clean it up
    const title = message.length > 50 ? message.substring(0, 47) + '...' : message

    // Clean up any newlines or extra whitespace
    return title.replace(/\s+/g, ' ').trim()
  }
}

export const chatService = new ChatService()
