export interface Message {
  id: string
  content: string
  role: 'user' | 'assistant' | 'system'
  timestamp: Date
  isTyping?: boolean
  artifacts?: string[]
  isStreaming?: boolean
  isComplete?: boolean
}

export interface ArtifactFile {
  id: string
  name: string
  content: string
  language: string
  type: 'file' | 'folder'
  children?: ArtifactFile[]
  isExpanded?: boolean
}

export interface ChatRequest {
  message: string
  session_id?: string
}

export interface ChatResponse {
  response: string
  session_id: string
  artifacts?: string[]
}

export interface StreamChunk {
  type: 'content' | 'artifacts' | 'complete' | 'error'
  content?: string
  artifacts?: string[]
  session_id?: string
  error?: string
}

export interface ApiError {
  message: string
  status?: number
}
