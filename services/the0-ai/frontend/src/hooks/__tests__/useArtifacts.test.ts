import React from 'react'
import { renderHook, waitFor } from '@testing-library/react'
import { describe, it, expect, vi, beforeEach, Mock } from 'vitest'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { useArtifacts } from '../useArtifacts'
import { useArtifactsStore } from '@/stores/artifactsStore'
import { useChatStore } from '@/stores/chatStore'
import { apiService } from '@/services/api'
import { ArtifactFile } from '@/types'

// Mock the dependencies
vi.mock('@/stores/artifactsStore')
vi.mock('@/stores/chatStore')
vi.mock('@/services/api')

const mockApiService = apiService as any

const mockArtifactNames = ['main.py', 'config.yaml', 'utils.py']

const mockArtifactResponses = {
  'main.py': {
    filename: 'main.py',
    content: 'import pandas as pd\\n\\ndef main():\\n    print("Hello World")',
    version: 1,
  },
  'config.yaml': {
    filename: 'config.yaml',
    content: 'debug: true\\nport: 3000',
    version: 1,
  },
  'utils.py': {
    filename: 'utils.py',
    content: 'def helper():\\n    return "help"',
    version: 1,
  },
}

const expectedFiles: ArtifactFile[] = [
  {
    id: 'main.py',
    name: 'main.py',
    content: 'import pandas as pd\n\ndef main():\n    print("Hello World")',
    language: 'python',
    type: 'file',
  },
  {
    id: 'config.yaml',
    name: 'config.yaml',
    content: 'debug: true\nport: 3000',
    language: 'yaml',
    type: 'file',
  },
  {
    id: 'utils.py',
    name: 'utils.py',
    content: 'def helper():\n    return "help"',
    language: 'python',
    type: 'file',
  },
]

function createWrapper() {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
      mutations: { retry: false },
    },
  })

  return ({ children }: { children: React.ReactNode }) =>
    React.createElement(QueryClientProvider, { client: queryClient }, children)
}

describe('useArtifacts', () => {
  const mockUseArtifactsStore = useArtifactsStore as unknown as Mock
  const mockUseChatStore = useChatStore as unknown as Mock

  const defaultStoreMock = {
    forceShow: true,
    setFiles: vi.fn(),
    files: [],
    setActiveFile: vi.fn(),
  }

  const defaultChatMock = {
    sessionId: 'test-session-123',
  }

  beforeEach(() => {
    vi.clearAllMocks()
    mockUseArtifactsStore.mockReturnValue(defaultStoreMock)
    mockUseChatStore.mockReturnValue(defaultChatMock)

    // Setup API mocks
    mockApiService.getSessionArtifacts = vi.fn()
    mockApiService.getArtifact = vi.fn()
  })

  describe('Basic functionality', () => {
    it('returns initial empty state', () => {
      mockApiService.getSessionArtifacts.mockResolvedValue([])

      const { result } = renderHook(() => useArtifacts(), {
        wrapper: createWrapper(),
      })

      expect(result.current.files).toEqual([])
      expect(result.current.isLoading).toBe(true) // Initially loading
      expect(result.current.error).toBe(null)
    })

    it('fetches and processes artifacts successfully', async () => {
      mockApiService.getSessionArtifacts.mockResolvedValue(mockArtifactNames)
      mockApiService.getArtifact.mockImplementation((filename: string) =>
        Promise.resolve(mockArtifactResponses[filename as keyof typeof mockArtifactResponses])
      )

      const setFiles = vi.fn()
      const setActiveFile = vi.fn()
      mockUseArtifactsStore.mockReturnValue({
        ...defaultStoreMock,
        setFiles,
        setActiveFile,
      })

      const { result } = renderHook(() => useArtifacts(), {
        wrapper: createWrapper(),
      })

      await waitFor(() => {
        expect(result.current.isLoading).toBe(false)
      })

      expect(mockApiService.getSessionArtifacts).toHaveBeenCalledWith('test-session-123')
      expect(mockApiService.getArtifact).toHaveBeenCalledTimes(3)
      expect(setFiles).toHaveBeenCalledWith(expectedFiles)
      // Active file should not be reset to null when files are updated
      expect(setActiveFile).not.toHaveBeenCalledWith(null)
    })

    it('does not fetch when forceShow is false', () => {
      mockUseArtifactsStore.mockReturnValue({
        ...defaultStoreMock,
        forceShow: false,
      })

      renderHook(() => useArtifacts(), {
        wrapper: createWrapper(),
      })

      expect(mockApiService.getSessionArtifacts).not.toHaveBeenCalled()
    })

    it('does not fetch when sessionId is missing', () => {
      mockUseChatStore.mockReturnValue({
        sessionId: null,
      })

      renderHook(() => useArtifacts(), {
        wrapper: createWrapper(),
      })

      expect(mockApiService.getSessionArtifacts).not.toHaveBeenCalled()
    })
  })

  describe('Content processing', () => {
    it('unescapes newlines and other escape sequences in content', async () => {
      const escapedContent = 'def test():\\n    print(\\"Hello\\")\\n    return True'
      const unescapedContent = 'def test():\n    print("Hello")\n    return True'

      mockApiService.getSessionArtifacts.mockResolvedValue(['test.py'])
      mockApiService.getArtifact.mockResolvedValue({
        filename: 'test.py',
        content: escapedContent,
        version: 1,
      })

      const setFiles = vi.fn()
      mockUseArtifactsStore.mockReturnValue({
        ...defaultStoreMock,
        setFiles,
      })

      renderHook(() => useArtifacts(), {
        wrapper: createWrapper(),
      })

      await waitFor(() => {
        expect(setFiles).toHaveBeenCalled()
      })

      const expectedFile = {
        id: 'test.py',
        name: 'test.py',
        content: unescapedContent,
        language: 'python',
        type: 'file',
      }

      expect(setFiles).toHaveBeenCalledWith([expectedFile])
    })

    it('handles content that is already properly formatted', async () => {
      const normalContent = 'def test():\n    return "normal"'

      mockApiService.getSessionArtifacts.mockResolvedValue(['normal.py'])
      mockApiService.getArtifact.mockResolvedValue({
        filename: 'normal.py',
        content: normalContent,
        version: 1,
      })

      const setFiles = vi.fn()
      mockUseArtifactsStore.mockReturnValue({
        ...defaultStoreMock,
        setFiles,
      })

      renderHook(() => useArtifacts(), {
        wrapper: createWrapper(),
      })

      await waitFor(() => {
        expect(setFiles).toHaveBeenCalled()
      })

      const expectedFile = {
        id: 'normal.py',
        name: 'normal.py',
        content: normalContent,
        language: 'python',
        type: 'file',
      }

      expect(setFiles).toHaveBeenCalledWith([expectedFile])
    })

    it('detects language from file extension correctly', async () => {
      const testCases = [
        { filename: 'script.js', expectedLanguage: 'javascript' },
        { filename: 'component.tsx', expectedLanguage: 'typescript' },
        { filename: 'config.yaml', expectedLanguage: 'yaml' },
        { filename: 'data.json', expectedLanguage: 'json' },
        { filename: 'README.md', expectedLanguage: 'markdown' },
        { filename: 'unknown.xyz', expectedLanguage: 'plaintext' },
      ]

      for (const { filename, expectedLanguage } of testCases) {
        mockApiService.getSessionArtifacts.mockResolvedValue([filename])
        mockApiService.getArtifact.mockResolvedValue({
          filename,
          content: 'test content',
          version: 1,
        })

        const setFiles = vi.fn()
        mockUseArtifactsStore.mockReturnValue({
          ...defaultStoreMock,
          setFiles,
        })

        const { unmount } = renderHook(() => useArtifacts(), {
          wrapper: createWrapper(),
        })

        await waitFor(() => {
          expect(setFiles).toHaveBeenCalled()
        })

        const expectedFile = {
          id: filename,
          name: filename,
          content: 'test content',
          language: expectedLanguage,
          type: 'file',
        }

        expect(setFiles).toHaveBeenCalledWith([expectedFile])

        unmount()
        vi.clearAllMocks()
        mockUseArtifactsStore.mockReturnValue(defaultStoreMock)
      }
    })
  })

  describe('Error handling', () => {
    it('handles API errors when fetching artifact list', async () => {
      const error = new Error('Failed to fetch artifacts')
      mockApiService.getSessionArtifacts.mockRejectedValue(error)

      const { result } = renderHook(() => useArtifacts(), {
        wrapper: createWrapper(),
      })

      await waitFor(() => {
        expect(result.current.error).toBe(error)
      })

      expect(result.current.isLoading).toBe(false)
      expect(result.current.files).toEqual([])
    })

    it('handles errors when fetching individual artifact content', async () => {
      mockApiService.getSessionArtifacts.mockResolvedValue(['good.py', 'bad.py'])
      mockApiService.getArtifact.mockImplementation((filename: string) => {
        if (filename === 'good.py') {
          return Promise.resolve({
            filename: 'good.py',
            content: 'def good(): pass',
            version: 1,
          })
        } else {
          return Promise.reject(new Error('Failed to fetch bad.py'))
        }
      })

      const setFiles = vi.fn()
      mockUseArtifactsStore.mockReturnValue({
        ...defaultStoreMock,
        setFiles,
      })

      renderHook(() => useArtifacts(), {
        wrapper: createWrapper(),
      })

      await waitFor(() => {
        expect(setFiles).toHaveBeenCalled()
      })

      // Should include the good file and a placeholder for the bad one
      const calledWith = setFiles.mock.calls[0][0]
      expect(calledWith).toHaveLength(2)
      expect(calledWith[0].name).toBe('good.py')
      expect(calledWith[0].content).toBe('def good(): pass')
      expect(calledWith[1].name).toBe('bad.py')
      expect(calledWith[1].content).toBe('// Failed to load content')
    })
  })

  describe('Store integration', () => {
    it('updates store files when artifacts change', async () => {
      mockApiService.getSessionArtifacts.mockResolvedValue(['test.py'])
      mockApiService.getArtifact.mockResolvedValue({
        filename: 'test.py',
        content: 'print("test")',
        version: 1,
      })

      const setFiles = vi.fn()
      mockUseArtifactsStore.mockReturnValue({
        ...defaultStoreMock,
        setFiles,
      })

      renderHook(() => useArtifacts(), {
        wrapper: createWrapper(),
      })

      await waitFor(() => {
        expect(setFiles).toHaveBeenCalled()
      })

      expect(setFiles).toHaveBeenCalledWith([
        {
          id: 'test.py',
          name: 'test.py',
          content: 'print("test")',
          language: 'python',
          type: 'file',
        },
      ])
    })

    it('preserves active file when updating files', async () => {
      mockApiService.getSessionArtifacts.mockResolvedValue(['test.py'])
      mockApiService.getArtifact.mockResolvedValue({
        filename: 'test.py',
        content: 'print("test")',
        version: 1,
      })

      const setActiveFile = vi.fn()
      const setFiles = vi.fn()
      mockUseArtifactsStore.mockReturnValue({
        ...defaultStoreMock,
        setActiveFile,
        setFiles,
      })

      renderHook(() => useArtifacts(), {
        wrapper: createWrapper(),
      })

      await waitFor(() => {
        expect(setFiles).toHaveBeenCalled()
      })

      // Active file should not be reset to null when files are updated
      expect(setActiveFile).not.toHaveBeenCalledWith(null)
    })

    it('returns files from store', () => {
      const storeFiles = [expectedFiles[0]]
      mockUseArtifactsStore.mockReturnValue({
        ...defaultStoreMock,
        files: storeFiles,
      })

      mockApiService.getSessionArtifacts.mockResolvedValue([])

      const { result } = renderHook(() => useArtifacts(), {
        wrapper: createWrapper(),
      })

      expect(result.current.files).toBe(storeFiles)
    })
  })

  describe('Session handling', () => {
    it('refetches when session ID changes', async () => {
      mockApiService.getSessionArtifacts.mockResolvedValue([])

      const { rerender } = renderHook(() => useArtifacts(), {
        wrapper: createWrapper(),
      })

      expect(mockApiService.getSessionArtifacts).toHaveBeenCalledWith('test-session-123')

      // Change session ID
      mockUseChatStore.mockReturnValue({
        sessionId: 'new-session-456',
      })

      rerender()

      await waitFor(() => {
        expect(mockApiService.getSessionArtifacts).toHaveBeenCalledWith('new-session-456')
      })

      expect(mockApiService.getSessionArtifacts).toHaveBeenCalledTimes(2)
    })

    it('handles session ID changing to null', () => {
      mockApiService.getSessionArtifacts.mockResolvedValue([])

      const { rerender } = renderHook(() => useArtifacts(), {
        wrapper: createWrapper(),
      })

      // Change to null session
      mockUseChatStore.mockReturnValue({
        sessionId: null,
      })

      rerender()

      // Should not make additional API calls
      expect(mockApiService.getSessionArtifacts).toHaveBeenCalledTimes(1)
    })
  })
})
