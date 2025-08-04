import { useRef, useEffect } from 'react'
import Editor from '@monaco-editor/react'
import { ArtifactFile } from '@/types'
import { useThemeStore } from '@/stores/themeStore'

interface CodeEditorProps {
  file: ArtifactFile
  onChange: (content: string) => void
  readOnly?: boolean
}

const getLanguageFromExtension = (fileName: string): string => {
  const extension = fileName.split('.').pop()?.toLowerCase()

  switch (extension) {
    case 'py':
      return 'python'
    case 'js':
    case 'jsx':
      return 'javascript'
    case 'ts':
    case 'tsx':
      return 'typescript'
    case 'yaml':
    case 'yml':
      return 'yaml'
    case 'json':
      return 'json'
    case 'md':
      return 'markdown'
    case 'html':
      return 'html'
    case 'css':
      return 'css'
    default:
      return 'plaintext'
  }
}

export function CodeEditor({ file, onChange, readOnly = false }: CodeEditorProps) {
  const editorRef = useRef<any>(null)
  const { isDark } = useThemeStore()
  const language = file.language || getLanguageFromExtension(file.name)

  const handleEditorDidMount = (editor: any) => {
    editorRef.current = editor
  }

  const handleEditorChange = (value: string | undefined) => {
    if (value !== undefined) {
      onChange(value)
    }
  }

  // Update theme when it changes
  useEffect(() => {
    if (editorRef.current) {
      const theme = isDark ? 'vs-dark' : 'vs-light'
      editorRef.current.updateOptions({ theme })
    }
  }, [isDark])

  // Force editor update when file changes
  useEffect(() => {
    if (editorRef.current && file.content !== editorRef.current.getValue()) {
      editorRef.current.setValue(file.content)
    }
  }, [file.id, file.content])

  return (
    <div className="h-full flex flex-col">
      <div className="border-b border-border p-3 bg-muted/30">
        <div className="flex items-center justify-between">
          <h3 className="font-medium text-sm">{file.name}</h3>
          <div className="text-xs text-muted-foreground uppercase">{language}</div>
        </div>
      </div>

      <div className="flex-1">
        <Editor
          key={file.id}
          height="100%"
          language={language}
          value={file.content}
          onChange={handleEditorChange}
          onMount={handleEditorDidMount}
          theme={isDark ? 'vs-dark' : 'vs-light'}
          options={{
            readOnly,
            minimap: { enabled: true },
            fontSize: 14,
            lineNumbers: 'on',
            roundedSelection: false,
            scrollBeyondLastLine: false,
            automaticLayout: true,
            tabSize: 2,
            insertSpaces: true,
            wordWrap: 'on',
          }}
        />
      </div>
    </div>
  )
}
