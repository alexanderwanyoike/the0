import { Header } from '@/components/layout/Header'
import { ChatPanel } from '@/components/chat/ChatPanel'
import { ChatList } from '@/components/chat/ChatList'
import { CollapsibleArtifactsPanel } from '@/components/layout/CollapsibleArtifactsPanel'
import { ApiKeySetup } from '@/components/setup/ApiKeySetup'
import { useAutoUpdater } from '@/components/AutoUpdater'
import { useArtifactsStore } from '@/stores/artifactsStore'
import { useArtifacts } from '@/hooks/useArtifacts'
import { useChatSessions } from '@/hooks/useChatSessions'
import { useThemeStore } from '@/stores/themeStore'
import { useEffect, useState } from 'react'
import { apiService } from '@/services/api'

function App() {
  const { forceShow } = useArtifactsStore()
  const { files } = useArtifacts()
  const { switchToSession, startNewChat } = useChatSessions()
  const { isDark } = useThemeStore()
  const shouldShowArtifacts = forceShow && files.length > 0

  // Initialize auto-updater (runs checks in background)
  useAutoUpdater()

  const [, setHasApiKey] = useState<boolean | null>(null)
  const [showApiKeySetup, setShowApiKeySetup] = useState(false)

  // Apply theme immediately on app load
  useEffect(() => {
    if (isDark) {
      document.documentElement.classList.add('dark')
    } else {
      document.documentElement.classList.remove('dark')
    }
  }, [isDark])

  // Handle zoom in/out with +/- keys
  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.ctrlKey || event.metaKey) {
        if (event.key === '=' || event.key === '+') {
          event.preventDefault()
          document.body.style.zoom = String(Math.min(2, parseFloat(document.body.style.zoom || '1') + 0.1))
        } else if (event.key === '-') {
          event.preventDefault()
          document.body.style.zoom = String(Math.max(0.5, parseFloat(document.body.style.zoom || '1') - 0.1))
        } else if (event.key === '0') {
          event.preventDefault()
          document.body.style.zoom = '1'
        }
      }
    }

    window.addEventListener('keydown', handleKeyDown)
    return () => window.removeEventListener('keydown', handleKeyDown)
  }, [])

  // Check API key status on startup
  useEffect(() => {
    const checkApiKeyStatus = async () => {
      try {
        const status = await apiService.checkApiKeyStatus()
        setHasApiKey(status.has_api_key)
        if (!status.has_api_key) {
          setShowApiKeySetup(true)
        }
      } catch (error) {
        console.error('Failed to check API key status:', error)
        // Show setup anyway if we can't check
        setShowApiKeySetup(true)
      }
    }

    checkApiKeyStatus()
  }, [])

  const handleApiKeySetupComplete = () => {
    setShowApiKeySetup(false)
    setHasApiKey(true)
  }

  return (
    <div className="h-screen flex flex-col bg-background">
      <Header />
      <div className="flex-1 flex overflow-hidden">
        {/* Chat List Sidebar */}
        <div className="w-80">
          <ChatList onSessionSelect={switchToSession} onNewChat={startNewChat} />
        </div>

        {/* Chat Panel */}
        <div className={`border-r border-border ${shouldShowArtifacts ? 'w-[40%]' : 'flex-1'}`}>
          <ChatPanel />
        </div>

        {/* Artifacts Panel */}
        <CollapsibleArtifactsPanel />
      </div>

      {/* API Key Setup Modal */}
      <ApiKeySetup open={showApiKeySetup} onComplete={handleApiKeySetupComplete} allowSkip={true} />
    </div>
  )
}

export default App
