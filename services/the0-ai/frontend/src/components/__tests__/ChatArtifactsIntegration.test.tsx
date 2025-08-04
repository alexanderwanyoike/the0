import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, vi, beforeEach, Mock } from 'vitest'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { ChatPanel } from '../chat/ChatPanel'
import { ArtifactsPanel } from '../artifacts/ArtifactsPanel'
import { useStreamingChat } from '@/hooks/useStreamingChat'
import { useArtifactsStore } from '@/stores/artifactsStore'
import { useArtifacts } from '@/hooks/useArtifacts'
import { useChatStore } from '@/stores/chatStore'
import { useSettingsStore } from '@/stores/settingsStore'
import { Message, ArtifactFile } from '@/types'

// Mock all the hooks and stores
vi.mock('@/hooks/useStreamingChat')
vi.mock('@/stores/artifactsStore')
vi.mock('@/hooks/useArtifacts')
vi.mock('@/stores/chatStore')
vi.mock('@/stores/settingsStore')

// Mock child components that we don't need to test in detail
vi.mock('../chat/MessageList', () => ({
  MessageList: vi.fn(({ messages }) => (
    <div data-testid="message-list">
      {messages.map((msg: Message) => (
        <div key={msg.id} data-testid={`message-${msg.id}`}>
          <div data-testid={`message-content-${msg.id}`}>{msg.content}</div>
          {msg.artifacts && (
            <div data-testid={`message-artifacts-${msg.id}`}>
              Artifacts: {msg.artifacts.join(', ')}
            </div>
          )}
        </div>
      ))}
    </div>
  )),
}))

vi.mock('../chat/ChatInput', () => ({
  ChatInput: vi.fn(({ onSendMessage, disabled }) => (
    <div data-testid="chat-input">
      <input
        data-testid="chat-input-field"
        placeholder="Type a message..."
        disabled={disabled}
        onKeyDown={e => {
          if (e.key === 'Enter' && e.currentTarget.value) {
            onSendMessage(e.currentTarget.value)
            e.currentTarget.value = ''
          }
        }}
      />
    </div>
  )),
}))

vi.mock('../artifacts/FileTree', () => ({
  FileTree: vi.fn(({ files, selectedFile, onFileSelect }) => (
    <div data-testid="file-tree">
      {files.map((file: ArtifactFile) => (
        <div
          key={file.id}
          data-testid={`file-item-${file.id}`}
          onClick={() => onFileSelect(file)}
          style={{
            backgroundColor: selectedFile?.id === file.id ? 'blue' : 'transparent',
          }}
        >
          {file.name}
        </div>
      ))}
    </div>
  )),
}))

vi.mock('../artifacts/CodeEditor', () => ({
  CodeEditor: vi.fn(({ file, onChange, readOnly }) => (
    <div data-testid="code-editor">
      <div data-testid="editor-file-name">{file.name}</div>
      <textarea
        data-testid="editor-content"
        value={file.content}
        onChange={e => onChange(e.target.value)}
        readOnly={readOnly}
      />
    </div>
  )),
}))

const mockMessages: Message[] = [
  {
    id: '1',
    content: 'Create a trading bot',
    role: 'user',
    timestamp: new Date(),
    isStreaming: false,
    isComplete: true,
  },
  {
    id: '2',
    content: "I'll help you create a trading bot. Here are the files:",
    role: 'assistant',
    timestamp: new Date(),
    artifacts: ['main.py', 'config.yaml'],
    isStreaming: false,
    isComplete: true,
  },
]

const mockFiles: ArtifactFile[] = [
  {
    id: '1',
    name: 'main.py',
    content: 'import pandas as pd\n\ndef trading_strategy():\n    pass',
    language: 'python',
    type: 'file',
  },
  {
    id: '2',
    name: 'config.yaml',
    content: 'strategy:\n  name: "momentum"\n  timeframe: "1h"',
    language: 'yaml',
    type: 'file',
  },
]

function TestWrapper({ children }: { children: React.ReactNode }) {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
      mutations: { retry: false },
    },
  })

  return <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
}

describe('Chat and Artifacts Integration', () => {
  const mockUseStreamingChat = useStreamingChat as unknown as Mock
  const mockUseArtifactsStore = useArtifactsStore as unknown as Mock
  const mockUseArtifacts = useArtifacts as unknown as Mock
  const mockUseChatStore = useChatStore as unknown as Mock
  const mockUseSettingsStore = useSettingsStore as unknown as Mock

  const defaultChatMock = {
    messages: [],
    sendMessage: vi.fn(),
    isStreaming: false,
  }

  const defaultStoreMock = {
    activeFile: null,
    setActiveFile: vi.fn(),
    updateFile: vi.fn(),
  }

  const defaultArtifactsMock = {
    files: [],
    isLoading: false,
    error: null,
  }

  beforeEach(() => {
    vi.clearAllMocks()
    mockUseStreamingChat.mockReturnValue(defaultChatMock)
    mockUseArtifactsStore.mockReturnValue(defaultStoreMock)
    mockUseArtifacts.mockReturnValue(defaultArtifactsMock)
    mockUseChatStore.mockReturnValue({ sessionId: 'test-session-123' })
    mockUseSettingsStore.mockReturnValue({ streamingEnabled: true })
  })

  describe('Chat to Artifacts Flow', () => {
    it('displays artifacts in chat messages when they are created', () => {
      mockUseStreamingChat.mockReturnValue({
        ...defaultChatMock,
        messages: mockMessages,
      })

      render(
        <TestWrapper>
          <ChatPanel />
        </TestWrapper>
      )

      // Check that message with artifacts is displayed
      expect(screen.getByTestId('message-2')).toBeInTheDocument()
      expect(screen.getByTestId('message-artifacts-2')).toHaveTextContent(
        'Artifacts: main.py, config.yaml'
      )
    })

    it('sends message through chat and triggers artifact generation', async () => {
      const sendMessage = vi.fn()
      mockUseStreamingChat.mockReturnValue({
        ...defaultChatMock,
        sendMessage,
      })

      render(
        <TestWrapper>
          <ChatPanel />
        </TestWrapper>
      )

      const input = screen.getByTestId('chat-input-field')
      fireEvent.keyDown(input, { key: 'Enter', target: { value: 'Create a trading bot' } })

      expect(sendMessage).toHaveBeenCalledWith('Create a trading bot', true)
    })

    it('handles streaming message updates with artifacts', async () => {
      const initialMessage: Message = {
        id: '3',
        content: 'Creating your trading bot...',
        role: 'assistant',
        timestamp: new Date(),
        isStreaming: true,
        isComplete: false,
      }

      const { rerender } = render(
        <TestWrapper>
          <ChatPanel />
        </TestWrapper>
      )

      // Initially no artifacts
      mockUseStreamingChat.mockReturnValue({
        ...defaultChatMock,
        messages: [initialMessage],
        isStreaming: true,
      })

      rerender(
        <TestWrapper>
          <ChatPanel />
        </TestWrapper>
      )

      expect(screen.getByTestId('message-3')).toBeInTheDocument()
      expect(screen.queryByTestId('message-artifacts-3')).not.toBeInTheDocument()

      // Simulate artifact chunk received
      const updatedMessage: Message = {
        id: '3',
        content: "Creating your trading bot... Here's your main.py file:",
        role: 'assistant',
        timestamp: new Date(),
        artifacts: ['main.py'],
        isStreaming: true,
        isComplete: false,
      }

      mockUseStreamingChat.mockReturnValue({
        ...defaultChatMock,
        messages: [updatedMessage],
        isStreaming: true,
      })

      rerender(
        <TestWrapper>
          <ChatPanel />
        </TestWrapper>
      )

      expect(screen.getByTestId('message-artifacts-3')).toHaveTextContent('Artifacts: main.py')
    })
  })

  describe('Artifacts Panel Integration', () => {
    it('displays files from artifacts hook in file tree', () => {
      mockUseArtifacts.mockReturnValue({
        ...defaultArtifactsMock,
        files: mockFiles,
      })

      render(
        <TestWrapper>
          <ArtifactsPanel />
        </TestWrapper>
      )

      expect(screen.getByTestId('file-tree')).toBeInTheDocument()
      expect(screen.getByTestId('file-item-1')).toHaveTextContent('main.py')
      expect(screen.getByTestId('file-item-2')).toHaveTextContent('config.yaml')
    })

    it('selects and displays file content when file is clicked', () => {
      const setActiveFile = vi.fn()
      mockUseArtifactsStore.mockReturnValue({
        ...defaultStoreMock,
        setActiveFile,
      })
      mockUseArtifacts.mockReturnValue({
        ...defaultArtifactsMock,
        files: mockFiles,
      })

      const { rerender } = render(
        <TestWrapper>
          <ArtifactsPanel />
        </TestWrapper>
      )

      // Click on a file
      fireEvent.click(screen.getByTestId('file-item-1'))
      expect(setActiveFile).toHaveBeenCalledWith(mockFiles[0])

      // Simulate store update
      mockUseArtifactsStore.mockReturnValue({
        ...defaultStoreMock,
        activeFile: mockFiles[0],
        setActiveFile,
      })

      rerender(
        <TestWrapper>
          <ArtifactsPanel />
        </TestWrapper>
      )

      // Should show code editor with file content
      expect(screen.getByTestId('code-editor')).toBeInTheDocument()
      expect(screen.getByTestId('editor-file-name')).toHaveTextContent('main.py')
      expect(screen.getByTestId('editor-content')).toHaveValue(mockFiles[0].content)
    })

    it('synchronizes file updates between chat session and artifacts', async () => {
      const updateFile = vi.fn()
      mockUseArtifactsStore.mockReturnValue({
        ...defaultStoreMock,
        activeFile: mockFiles[0],
        updateFile,
      })
      mockUseArtifacts.mockReturnValue({
        ...defaultArtifactsMock,
        files: mockFiles,
      })

      render(
        <TestWrapper>
          <ArtifactsPanel />
        </TestWrapper>
      )

      // Modify file content in editor
      const editor = screen.getByTestId('editor-content')
      const newContent = 'import pandas as pd\n\ndef improved_strategy():\n    return "profit"'

      fireEvent.change(editor, { target: { value: newContent } })

      expect(updateFile).toHaveBeenCalledWith(mockFiles[0].id, newContent)
    })
  })

  describe('End-to-End Chat to Artifacts Workflow', () => {
    it('completes full workflow from chat message to artifact display', async () => {
      const sendMessage = vi.fn()
      const setActiveFile = vi.fn()

      // Setup initial state
      mockUseStreamingChat.mockReturnValue({
        messages: [],
        sendMessage,
        isStreaming: false,
      })
      mockUseArtifactsStore.mockReturnValue({
        ...defaultStoreMock,
        setActiveFile,
      })
      mockUseArtifacts.mockReturnValue(defaultArtifactsMock)

      const ChatAndArtifacts = () => (
        <TestWrapper>
          <div style={{ display: 'flex', height: '100vh' }}>
            <div style={{ width: '50%' }}>
              <ChatPanel />
            </div>
            <div style={{ width: '50%' }}>
              <ArtifactsPanel />
            </div>
          </div>
        </TestWrapper>
      )

      const { rerender } = render(<ChatAndArtifacts />)

      // Step 1: Send a message
      const input = screen.getByTestId('chat-input-field')
      fireEvent.keyDown(input, {
        key: 'Enter',
        target: { value: 'Create a momentum trading strategy' },
      })

      expect(sendMessage).toHaveBeenCalledWith('Create a momentum trading strategy', true)

      // Step 2: Simulate response with artifacts
      mockUseStreamingChat.mockReturnValue({
        messages: [
          {
            id: '1',
            content: 'Create a momentum trading strategy',
            role: 'user',
            timestamp: new Date(),
            isStreaming: false,
            isComplete: true,
          },
          {
            id: '2',
            content: "I've created a momentum trading strategy for you with the following files:",
            role: 'assistant',
            timestamp: new Date(),
            artifacts: ['momentum_strategy.py', 'config.yaml'],
            isStreaming: false,
            isComplete: true,
          },
        ],
        sendMessage,
        isStreaming: false,
      })

      // Step 3: Simulate artifacts being loaded
      mockUseArtifacts.mockReturnValue({
        files: mockFiles,
        isLoading: false,
        error: null,
      })

      rerender(<ChatAndArtifacts />)

      // Verify chat shows artifacts
      expect(screen.getByTestId('message-artifacts-2')).toHaveTextContent(
        'Artifacts: momentum_strategy.py, config.yaml'
      )

      // Verify artifacts panel shows files
      expect(screen.getByTestId('file-tree')).toBeInTheDocument()
      expect(screen.getByTestId('file-item-1')).toHaveTextContent('main.py')

      // Step 4: Click on file to view content
      fireEvent.click(screen.getByTestId('file-item-1'))
      expect(setActiveFile).toHaveBeenCalledWith(mockFiles[0])

      // Step 5: Simulate file selection
      mockUseArtifactsStore.mockReturnValue({
        activeFile: mockFiles[0],
        setActiveFile,
        updateFile: vi.fn(),
      })

      rerender(<ChatAndArtifacts />)

      // Verify file content is displayed
      expect(screen.getByTestId('code-editor')).toBeInTheDocument()
      expect(screen.getByTestId('editor-file-name')).toHaveTextContent('main.py')
    })

    it('handles errors gracefully in the workflow', async () => {
      // Test error in artifacts loading
      mockUseArtifacts.mockReturnValue({
        files: [],
        isLoading: false,
        error: new Error('Failed to load artifacts'),
      })

      render(
        <TestWrapper>
          <ArtifactsPanel />
        </TestWrapper>
      )

      expect(screen.getByText('Error loading artifacts')).toBeInTheDocument()
      expect(screen.getByText('Failed to load artifacts')).toBeInTheDocument()
    })

    it('maintains state consistency across chat and artifacts', () => {
      // Test that session ID is consistent
      const sessionId = 'test-session-456'
      mockUseChatStore.mockReturnValue({ sessionId })

      render(
        <TestWrapper>
          <div>
            <ChatPanel />
            <ArtifactsPanel />
          </div>
        </TestWrapper>
      )

      // Both components should render correctly
      expect(screen.getByTestId('chat-input')).toBeInTheDocument()
      expect(
        screen.getByText('Start chatting with the0 to create trading bot files')
      ).toBeInTheDocument()
    })
  })
})
