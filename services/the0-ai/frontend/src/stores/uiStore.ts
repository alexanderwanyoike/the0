import { create } from 'zustand'

type Theme = 'light' | 'dark' | 'system'

interface UIStore {
  theme: Theme
  sidebarWidth: number
  isFileTreeCollapsed: boolean
  setTheme: (theme: Theme) => void
  setSidebarWidth: (width: number) => void
  toggleFileTree: () => void
  setFileTreeCollapsed: (collapsed: boolean) => void
}

export const useUIStore = create<UIStore>(set => ({
  theme: 'system',
  sidebarWidth: 400,
  isFileTreeCollapsed: false,

  setTheme: theme => {
    set(() => ({ theme }))

    // Apply theme to document
    const root = document.documentElement

    if (theme === 'dark') {
      root.classList.add('dark')
    } else if (theme === 'light') {
      root.classList.remove('dark')
    } else {
      // System theme
      const systemTheme = window.matchMedia('(prefers-color-scheme: dark)').matches
      if (systemTheme) {
        root.classList.add('dark')
      } else {
        root.classList.remove('dark')
      }
    }
  },

  setSidebarWidth: width => set(() => ({ sidebarWidth: Math.max(280, Math.min(600, width)) })),

  toggleFileTree: () => set(state => ({ isFileTreeCollapsed: !state.isFileTreeCollapsed })),

  setFileTreeCollapsed: collapsed => set(() => ({ isFileTreeCollapsed: collapsed })),
}))
