import React, { useState } from 'react'
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
  DialogDescription,
} from '@/components/ui/dialog'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
import { ExternalLink, Key, AlertCircle } from 'lucide-react'
import { apiService } from '@/services/api'

interface ApiKeySetupProps {
  open: boolean
  onComplete: () => void
  allowSkip?: boolean
}

export function ApiKeySetup({ open, onComplete, allowSkip = false }: ApiKeySetupProps) {
  const [apiKey, setApiKey] = useState('')
  const [isLoading, setIsLoading] = useState(false)
  const [error, setError] = useState('')

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()

    if (!apiKey.trim()) {
      setError('Please enter an API key')
      return
    }

    setIsLoading(true)
    setError('')

    try {
      await apiService.setApiKey(apiKey.trim())
      onComplete()
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to save API key')
    } finally {
      setIsLoading(false)
    }
  }

  const openGoogleAIStudio = () => {
    window.open('https://aistudio.google.com/apikey', '_blank')
  }

  return (
    <Dialog open={open} onOpenChange={() => {}}>
      <DialogContent className="sm:max-w-[500px]" onPointerDownOutside={e => e.preventDefault()}>
        <DialogHeader>
          <DialogTitle className="flex items-center gap-2">
            <Key className="h-5 w-5" />
            Setup Google AI API Key
          </DialogTitle>
          <DialogDescription>
            To use the0 AI Agent Workbench, you need to provide a Google AI API key. This allows the
            agent to access Gemini models for building trading bots.
          </DialogDescription>
        </DialogHeader>

        <div className="space-y-4">
          <div className="bg-blue-50 dark:bg-blue-950 p-4 rounded-lg">
            <div className="flex items-start gap-3">
              <AlertCircle className="h-5 w-5 text-blue-600 dark:text-blue-400 mt-0.5 flex-shrink-0" />
              <div className="space-y-2 text-sm">
                <p className="font-medium text-blue-900 dark:text-blue-100">
                  How to get your API key:
                </p>
                <ol className="list-decimal list-inside space-y-1 text-blue-800 dark:text-blue-200">
                  <li>Click the button below to visit Google AI Studio</li>
                  <li>Sign in with your Google account</li>
                  <li>Click "Create API Key" and copy it</li>
                  <li>Paste it in the field below</li>
                </ol>
              </div>
            </div>
          </div>

          <Button type="button" variant="outline" onClick={openGoogleAIStudio} className="w-full">
            <ExternalLink className="h-4 w-4 mr-2" />
            Get API Key from Google AI Studio
          </Button>

          <form onSubmit={handleSubmit} className="space-y-4">
            <div className="space-y-2">
              <Label htmlFor="apiKey">Google AI API Key</Label>
              <Input
                id="apiKey"
                type="password"
                placeholder="AIza..."
                value={apiKey}
                onChange={e => setApiKey(e.target.value)}
                className="font-mono"
              />
            </div>

            {error && <div className="text-sm text-red-600 dark:text-red-400">{error}</div>}

            <div className="flex gap-2">
              <Button type="submit" disabled={isLoading} className="flex-1">
                {isLoading ? 'Saving...' : 'Save API Key'}
              </Button>
              {allowSkip && (
                <Button type="button" variant="outline" onClick={onComplete}>
                  Skip for now
                </Button>
              )}
            </div>
          </form>

          <div className="text-xs text-gray-600 dark:text-gray-400">
            <p>🔒 Your API key is stored locally and never shared with external services.</p>
          </div>
        </div>
      </DialogContent>
    </Dialog>
  )
}
