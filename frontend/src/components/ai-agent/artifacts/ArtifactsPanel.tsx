import { FileTree } from "./FileTree";
import { CodeEditor } from "./CodeEditor";
import { useArtifactsStore } from "@/stores/ai-agent/artifactsStore";
import { useArtifacts } from "@/hooks/ai-agent/useArtifacts";

export function ArtifactsPanel() {
  const { activeFile, setActiveFile, updateFile } = useArtifactsStore();
  const { files, isLoading, error } = useArtifacts();

  if (isLoading) {
    return (
      <div className="h-full flex items-center justify-center">
        <div className="text-center">
          <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-primary mx-auto mb-4"></div>
          <p className="text-muted-foreground">Loading artifacts...</p>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="h-full flex items-center justify-center">
        <div className="text-center">
          <p className="text-lg mb-2 text-destructive">
            Error loading artifacts
          </p>
          <p className="text-sm text-muted-foreground">
            {error instanceof Error ? error.message : "Unknown error"}
          </p>
        </div>
      </div>
    );
  }

  return (
    <div className="h-full flex bg-background">
      <div className="w-80 border-r border-border">
        {files.length > 0 ? (
          <FileTree
            files={files}
            selectedFile={activeFile}
            onFileSelect={setActiveFile}
          />
        ) : (
          <div className="p-4 text-center text-muted-foreground">
            <p className="text-sm">
              Start chatting with the0 to create trading bot files
            </p>
          </div>
        )}
      </div>

      <div className="flex-1">
        {activeFile ? (
          <CodeEditor
            file={activeFile}
            onChange={(content) => {
              updateFile(activeFile.id, content);
            }}
            readOnly={true}
          />
        ) : (
          <div className="h-full flex items-center justify-center text-muted-foreground">
            <div className="text-center">
              <p className="text-lg mb-2">No file selected</p>
              <p className="text-sm">
                {files.length > 0
                  ? "Select a file from the tree to view its contents"
                  : "Chat with the0 AI agent to generate trading bot files"}
              </p>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
