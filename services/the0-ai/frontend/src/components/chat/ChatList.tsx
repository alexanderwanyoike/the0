import { useState, useEffect } from 'react'
import { Button } from '@/components/ui/button'
import { ScrollArea } from '@/components/ui/scroll-area'
import { MessageSquare, Plus, MoreHorizontal, Trash, Edit, Calendar } from 'lucide-react'
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
} from '@/components/ui/dropdown-menu'
import { Input } from '@/components/ui/input'
import { useChatStore } from '@/stores/chatStore'
import { chatService, ChatSession } from '@/services/chatService'
import { cn } from '@/lib/utils'

interface ChatListProps {
  onSessionSelect: (sessionId: string) => void
  onNewChat: () => void
}

export function ChatList({ onSessionSelect, onNewChat }: ChatListProps) {
  const {
    sessions,
    setSessions,
    currentSessionId,
    setCurrentSession,
    updateSessionTitle,
    removeSession,
  } = useChatStore()

  const [isLoading, setIsLoading] = useState(false)
  const [editingSessionId, setEditingSessionId] = useState<string | null>(null)
  const [editTitle, setEditTitle] = useState('')

  // Load sessions on mount
  useEffect(() => {
    loadSessions()
  }, [])

  const loadSessions = async () => {
    try {
      setIsLoading(true)
      const sessionsData = await chatService.getSessions()
      setSessions(sessionsData)
    } catch (error) {
      console.error('Failed to load sessions:', error)
    } finally {
      setIsLoading(false)
    }
  }

  const handleSessionClick = (session: ChatSession) => {
    setCurrentSession(session.id)
    onSessionSelect(session.id)
  }

  const handleNewChat = () => {
    setCurrentSession('')
    onNewChat()
  }

  const handleEditTitle = (session: ChatSession) => {
    setEditingSessionId(session.id)
    setEditTitle(session.title)
  }

  const handleSaveTitle = async (sessionId: string) => {
    if (editTitle.trim() && editTitle !== sessions.find(s => s.id === sessionId)?.title) {
      try {
        await chatService.updateSessionTitle(sessionId, editTitle.trim())
        updateSessionTitle(sessionId, editTitle.trim())
      } catch (error) {
        console.error('Failed to update title:', error)
      }
    }
    setEditingSessionId(null)
    setEditTitle('')
  }

  const handleDeleteSession = async (session: ChatSession) => {
    if (window.confirm(`Are you sure you want to delete "${session.title}"?`)) {
      try {
        await chatService.deleteSession(session.id)
        removeSession(session.id)

        // If we deleted the current session, start a new chat
        if (currentSessionId === session.id) {
          handleNewChat()
        }
      } catch (error) {
        console.error('Failed to delete session:', error)
      }
    }
  }

  const formatDate = (dateString: string) => {
    const date = new Date(dateString)
    const now = new Date()
    const diffTime = now.getTime() - date.getTime()
    const diffDays = Math.floor(diffTime / (1000 * 60 * 60 * 24))

    if (diffDays === 0) {
      return date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })
    } else if (diffDays === 1) {
      return 'Yesterday'
    } else if (diffDays < 7) {
      return `${diffDays} days ago`
    } else {
      return date.toLocaleDateString()
    }
  }

  return (
    <div className="h-full flex flex-col bg-muted/30 border-r border-border">
      {/* Header */}
      <div className="p-4 border-b border-border">
        <Button onClick={handleNewChat} className="w-full justify-start gap-2" variant="outline">
          <Plus className="h-4 w-4" />
          New Chat
        </Button>
      </div>

      {/* Sessions List */}
      <ScrollArea className="flex-1">
        <div className="p-2 space-y-1">
          {isLoading ? (
            <div className="p-4 text-center text-muted-foreground">
              <div className="animate-spin h-4 w-4 border-2 border-primary border-t-transparent rounded-full mx-auto mb-2" />
              Loading chats...
            </div>
          ) : sessions.length === 0 ? (
            <div className="p-4 text-center text-muted-foreground">
              <MessageSquare className="h-8 w-8 mx-auto mb-2 opacity-50" />
              <p className="text-sm">No chats yet</p>
              <p className="text-xs opacity-75">Start a new conversation</p>
            </div>
          ) : (
            sessions.map(session => (
              <div
                key={session.id}
                className={cn(
                  'group relative rounded-lg border border-transparent hover:border-border transition-all duration-200',
                  currentSessionId === session.id && 'bg-accent border-border'
                )}
              >
                <div
                  className="flex items-center p-3 cursor-pointer"
                  onClick={() => handleSessionClick(session)}
                >
                  <MessageSquare className="h-4 w-4 mr-3 flex-shrink-0 text-muted-foreground" />

                  <div className="flex-1 min-w-0">
                    {editingSessionId === session.id ? (
                      <Input
                        value={editTitle}
                        onChange={e => setEditTitle(e.target.value)}
                        onBlur={() => handleSaveTitle(session.id)}
                        onKeyDown={e => {
                          if (e.key === 'Enter') {
                            handleSaveTitle(session.id)
                          } else if (e.key === 'Escape') {
                            setEditingSessionId(null)
                            setEditTitle('')
                          }
                        }}
                        className="h-6 text-sm"
                        autoFocus
                        onClick={e => e.stopPropagation()}
                      />
                    ) : (
                      <>
                        <div className="text-sm font-medium truncate">{session.title}</div>
                        <div className="flex items-center gap-1 text-xs text-muted-foreground mt-1">
                          <Calendar className="h-3 w-3" />
                          {formatDate(session.updated_at)}
                        </div>
                      </>
                    )}
                  </div>

                  {editingSessionId !== session.id && (
                    <DropdownMenu>
                      <DropdownMenuTrigger asChild>
                        <Button
                          variant="ghost"
                          size="sm"
                          className="h-6 w-6 p-0 opacity-0 group-hover:opacity-100 transition-opacity"
                          onClick={e => e.stopPropagation()}
                        >
                          <MoreHorizontal className="h-3 w-3" />
                        </Button>
                      </DropdownMenuTrigger>
                      <DropdownMenuContent align="end">
                        <DropdownMenuItem
                          onClick={(e: React.MouseEvent) => {
                            e.stopPropagation()
                            handleEditTitle(session)
                          }}
                        >
                          <Edit className="h-3 w-3 mr-2" />
                          Rename
                        </DropdownMenuItem>
                        <DropdownMenuItem
                          onClick={(e: React.MouseEvent) => {
                            e.stopPropagation()
                            handleDeleteSession(session)
                          }}
                          className="text-destructive"
                        >
                          <Trash className="h-3 w-3 mr-2" />
                          Delete
                        </DropdownMenuItem>
                      </DropdownMenuContent>
                    </DropdownMenu>
                  )}
                </div>
              </div>
            ))
          )}
        </div>
      </ScrollArea>
    </div>
  )
}
