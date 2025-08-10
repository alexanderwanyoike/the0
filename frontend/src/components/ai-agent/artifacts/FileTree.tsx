import { ScrollArea } from "@/components/ui/scroll-area";
import { FileTreeItem } from "./FileTreeItem";
import { ArtifactFile } from "@/types";

interface FileTreeProps {
  files: ArtifactFile[];
  selectedFile: ArtifactFile | null;
  onFileSelect: (file: ArtifactFile) => void;
}

export function FileTree({ files, selectedFile, onFileSelect }: FileTreeProps) {
  return (
    <ScrollArea className="h-[calc(100vh-8rem)]">
      <div className="p-2">
        {files.map((file) => (
          <FileTreeItem
            key={file.id}
            file={file}
            selectedFile={selectedFile}
            onFileSelect={onFileSelect}
            level={0}
          />
        ))}
      </div>
    </ScrollArea>
  );
}
