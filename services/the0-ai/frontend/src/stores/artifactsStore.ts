import { create } from 'zustand'
import { ArtifactFile } from '@/types'

interface ArtifactsStore {
  files: ArtifactFile[]
  activeFile: ArtifactFile | null
  selectedFiles: string[]
  forceShow: boolean
  setFiles: (files: ArtifactFile[]) => void
  updateFile: (id: string, content: string) => void
  deleteFile: (id: string) => void
  setActiveFile: (file: ArtifactFile | null) => void
  addSelectedFile: (id: string) => void
  removeSelectedFile: (id: string) => void
  clearSelectedFiles: () => void
  setForceShow: (show: boolean) => void
}

const updateFileInTree = (files: ArtifactFile[], id: string, content: string): ArtifactFile[] => {
  return files.map(file => {
    if (file.id === id) {
      return { ...file, content }
    }
    if (file.children) {
      return {
        ...file,
        children: updateFileInTree(file.children, id, content),
      }
    }
    return file
  })
}

const removeFileFromTree = (files: ArtifactFile[], id: string): ArtifactFile[] => {
  return files
    .filter(file => file.id !== id)
    .map(file => {
      if (file.children) {
        return {
          ...file,
          children: removeFileFromTree(file.children, id),
        }
      }
      return file
    })
}

export const useArtifactsStore = create<ArtifactsStore>(set => ({
  files: [],
  activeFile: null,
  selectedFiles: [],
  forceShow: false,

  setFiles: files => {
    set(state => ({
      files,
      activeFile: (state.activeFile && files.find(f => f.id === state.activeFile?.id)) || null,
    }))
  },

  updateFile: (id, content) =>
    set(state => ({
      files: updateFileInTree(state.files, id, content),
    })),

  deleteFile: id =>
    set(state => ({
      files: removeFileFromTree(state.files, id),
      activeFile: state.activeFile?.id === id ? null : state.activeFile,
      selectedFiles: state.selectedFiles.filter(fileId => fileId !== id),
    })),

  setActiveFile: file => {
    set(() => ({ activeFile: file }))
  },

  addSelectedFile: id =>
    set(state => ({
      selectedFiles: state.selectedFiles.includes(id)
        ? state.selectedFiles
        : [...state.selectedFiles, id],
    })),

  removeSelectedFile: id =>
    set(state => ({
      selectedFiles: state.selectedFiles.filter(fileId => fileId !== id),
    })),

  clearSelectedFiles: () => set(() => ({ selectedFiles: [] })),

  setForceShow: show => set(() => ({ forceShow: show })),
}))
