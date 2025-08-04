import { useEffect, useState } from 'react'
import { toast } from 'sonner'
import { Download, RefreshCw, AlertCircle, CheckCircle } from 'lucide-react'
import { apiService } from '@/services/api'

interface UpdateInfo {
  update_available: boolean
  current_version: string
  latest_version?: string
  release_date?: string
  download_url?: string
  error?: string
}

export function AutoUpdater() {
  const [updateInfo, setUpdateInfo] = useState<UpdateInfo | null>(null)
  const [isUpdating, setIsUpdating] = useState(false)
  const [checkingForUpdates, setCheckingForUpdates] = useState(false)

  const checkForUpdates = async (showToastOnNoUpdate = false) => {
    try {
      setCheckingForUpdates(true)
      const result = await apiService.checkForUpdates()
      setUpdateInfo(result)

      if (result.error) {
        toast.error('Update Check Failed', {
          description: result.error,
          icon: <AlertCircle className="h-4 w-4" />,
        })
        return
      }

      if (result.update_available) {
        // Show update available toast
        toast.info('Update Available!', {
          description: `Version ${result.latest_version} is available. Click to update.`,
          icon: <Download className="h-4 w-4" />,
          action: {
            label: 'Update Now',
            onClick: () => handleInstallUpdate(),
          },
          duration: 10000, // Keep visible longer
        })
      } else if (showToastOnNoUpdate) {
        toast.success('Up to Date', {
          description: `You're running the latest version (${result.current_version})`,
          icon: <CheckCircle className="h-4 w-4" />,
        })
      }
    } catch (error) {
      console.error('Error checking for updates:', error)
      toast.error('Update Check Failed', {
        description: 'Failed to check for updates. Please try again later.',
        icon: <AlertCircle className="h-4 w-4" />,
      })
    } finally {
      setCheckingForUpdates(false)
    }
  }

  const handleInstallUpdate = async () => {
    if (!updateInfo?.update_available) {
      return
    }

    try {
      setIsUpdating(true)

      // Show installation started toast
      toast.loading('Installing Update', {
        description: `Downloading and installing version ${updateInfo.latest_version}...`,
        icon: <Download className="h-4 w-4" />,
      })

      const result = await apiService.installUpdate()

      if (result.success) {
        // Show success toast with restart prompt
        toast.success('Update Installed!', {
          description:
            'The update has been installed successfully. Please restart the application to use the new version.',
          icon: <CheckCircle className="h-4 w-4" />,
          action: {
            label: 'Restart Now',
            onClick: () => {
              // In a real desktop app, this would trigger a restart
              toast.info('Restart Required', {
                description: 'Please close and reopen the application to complete the update.',
                duration: Infinity,
              })
            },
          },
          duration: Infinity, // Keep visible until user takes action
        })
      } else {
        toast.error('Update Failed', {
          description: result.error || 'Failed to install update. Please try again later.',
          icon: <AlertCircle className="h-4 w-4" />,
        })
      }
    } catch (error) {
      console.error('Error installing update:', error)
      toast.error('Update Failed', {
        description: 'Failed to install update. Please try again later.',
        icon: <AlertCircle className="h-4 w-4" />,
      })
    } finally {
      setIsUpdating(false)
    }
  }

  // Check for updates on component mount
  useEffect(() => {
    // Check for updates silently on startup (don't show toast if no update)
    checkForUpdates(false)

    // Set up periodic update checks (every 2 hours)
    const interval = setInterval(
      () => {
        checkForUpdates(false)
      },
      2 * 60 * 60 * 1000
    ) // 2 hours in milliseconds

    return () => clearInterval(interval)
  }, [])

  // Manual update check button (for settings page)
  const UpdateButton = () => (
    <button
      onClick={() => checkForUpdates(true)}
      disabled={checkingForUpdates || isUpdating}
      className="inline-flex items-center px-3 py-2 text-sm font-medium text-gray-700 bg-white border border-gray-300 rounded-md hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 disabled:opacity-50"
    >
      <RefreshCw className={`h-4 w-4 mr-2 ${checkingForUpdates ? 'animate-spin' : ''}`} />
      {checkingForUpdates ? 'Checking...' : 'Check for Updates'}
    </button>
  )

  return {
    updateInfo,
    isUpdating,
    checkingForUpdates,
    checkForUpdates,
    handleInstallUpdate,
    UpdateButton,
  }
}

// Export a simple hook for easy use in other components
export function useAutoUpdater() {
  return AutoUpdater()
}
