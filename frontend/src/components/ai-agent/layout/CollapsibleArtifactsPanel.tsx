import { ArtifactsPanel } from "@/components/ai-agent/artifacts/ArtifactsPanel";
import { ArtifactToolbar } from "@/components/ai-agent/artifacts/ArtifactToolbar";
import { useArtifacts } from "@/hooks/ai-agent/useArtifacts";
import { useArtifactsStore } from "@/stores/ai-agent/artifactsStore";

export function CollapsibleArtifactsPanel() {
  const { files } = useArtifacts();
  const { forceShow, setForceShow } = useArtifactsStore();

  const shouldShow = forceShow && files.length > 0;

  if (!shouldShow) {
    return null; // Only show when explicitly requested and files exist
  }

  return (
    <div className="w-[70%] border-l border-border bg-background">
      <div className="h-full flex flex-col">
        <div className="border-b border-border p-4 flex items-center justify-between">
          <div>
            <h2 className="font-semibold text-lg">Bot Files</h2>
            <p className="text-sm text-muted-foreground">
              {files.length} artifact{files.length !== 1 ? "s" : ""} available
            </p>
          </div>
          <ArtifactToolbar files={files} onClose={() => setForceShow(false)} />
        </div>

        <div className="flex-1 overflow-hidden">
          <ArtifactsPanel />
        </div>
      </div>
    </div>
  );
}
