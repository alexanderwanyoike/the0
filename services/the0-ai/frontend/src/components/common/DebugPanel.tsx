import { useState } from 'react'
import { Button } from '@/components/ui/button'
import { apiService } from '@/services/api'
import { useArtifactsStore } from '@/stores/artifactsStore'

export function DebugPanel() {
  const [status, setStatus] = useState<string>('')
  const { setFiles } = useArtifactsStore()

  const testDirect = async () => {
    try {
      setStatus('Testing direct backend connection...')
      const response = await fetch('http://localhost:8000/health')
      const data = await response.json()
      setStatus(`✅ Direct backend: ${data.message}`)
    } catch (error) {
      setStatus(
        `❌ Direct backend error: ${error instanceof Error ? error.message : 'Backend not running?'}`
      )
    }
  }

  const testProxy = async () => {
    try {
      setStatus('Testing proxy connection...')
      const response = await fetch('/api/health')
      const data = await response.json()
      setStatus(`✅ Proxy working: ${data.message}`)
    } catch (error) {
      setStatus(`❌ Proxy error: ${error instanceof Error ? error.message : 'Proxy not working'}`)
    }
  }

  const testBackend = async () => {
    try {
      setStatus('Testing backend via service...')
      const health = await apiService.healthCheck()
      setStatus(`✅ Service working: ${health.message}`)
    } catch (error) {
      setStatus(`❌ Service error: ${error instanceof Error ? error.message : 'Unknown error'}`)
    }
  }

  const testChat = async () => {
    try {
      setStatus('Testing chat endpoint...')
      const response = await apiService.sendMessage({
        message: 'Hello test',
        session_id: undefined,
      })
      setStatus(`✅ Chat working: ${response.response}`)
    } catch (error) {
      setStatus(`❌ Chat error: ${error instanceof Error ? error.message : 'Unknown error'}`)
    }
  }

  const addMockArtifacts = () => {
    const mockFiles = [
      {
        id: 'test-1',
        name: 'main.py',
        content: '# Trading Bot Main File\nprint("Hello from main.py")',
        language: 'python',
        type: 'file' as const,
      },
      {
        id: 'test-2',
        name: 'bot-config.yaml',
        content: 'name: "Test Bot"\nversion: "1.0.0"',
        language: 'yaml',
        type: 'file' as const,
      },
      {
        id: 'test-3',
        name: 'requirements.txt',
        content: 'pandas==2.0.0\nnumpy==1.24.0',
        language: 'plaintext',
        type: 'file' as const,
      },
    ]
    setFiles(mockFiles)
    setStatus('✅ Added test artifacts!')
  }

  const clearArtifacts = () => {
    setFiles([])
    setStatus('✅ Cleared all artifacts!')
  }

  return (
    <div className="fixed bottom-4 right-4 bg-card border rounded-lg p-4 z-50 max-w-sm">
      <div className="text-sm font-medium mb-2">Debug Panel</div>
      <div className="space-y-2">
        <Button size="sm" onClick={testDirect} className="w-full text-xs">
          Direct Backend
        </Button>
        <Button size="sm" onClick={testProxy} className="w-full text-xs">
          Test Proxy
        </Button>
        <Button size="sm" onClick={testBackend} className="w-full text-xs">
          Test Service
        </Button>
        <Button size="sm" onClick={testChat} className="w-full text-xs">
          Test Chat
        </Button>
        <Button
          size="sm"
          onClick={addMockArtifacts}
          className="w-full text-xs bg-green-600 hover:bg-green-700"
        >
          Test Artifacts
        </Button>
        <Button
          size="sm"
          onClick={clearArtifacts}
          className="w-full text-xs bg-red-600 hover:bg-red-700"
        >
          Clear Artifacts
        </Button>
      </div>
      {status && <div className="mt-2 text-xs bg-muted p-2 rounded">{status}</div>}
    </div>
  )
}
