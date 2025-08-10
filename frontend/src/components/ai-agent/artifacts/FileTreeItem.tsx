import { useState } from "react";
import { Button } from "@/components/ui/button";
import {
  ChevronRight,
  ChevronDown,
  File,
  Folder,
  FolderOpen,
  FileText,
  Settings,
} from "lucide-react";
import { ArtifactFile } from "@/types";
import { cn } from "@/lib/utils";

interface FileTreeItemProps {
  file: ArtifactFile;
  selectedFile: ArtifactFile | null;
  onFileSelect: (file: ArtifactFile) => void;
  level: number;
}

const getFileIcon = (fileName: string, isFolder: boolean, isOpen?: boolean) => {
  if (isFolder) {
    return isOpen ? (
      <FolderOpen className="h-4 w-4" />
    ) : (
      <Folder className="h-4 w-4" />
    );
  }

  const extension = fileName.split(".").pop()?.toLowerCase();

  switch (extension) {
    case "py":
      return <FileText className="h-4 w-4 text-blue-500" />;
    case "yaml":
    case "yml":
      return <Settings className="h-4 w-4 text-orange-500" />;
    case "json":
      return <FileText className="h-4 w-4 text-yellow-500" />;
    case "md":
      return <FileText className="h-4 w-4 text-green-500" />;
    default:
      return <File className="h-4 w-4" />;
  }
};

export function FileTreeItem({
  file,
  selectedFile,
  onFileSelect,
  level,
}: FileTreeItemProps) {
  const [isExpanded, setIsExpanded] = useState(file.isExpanded ?? false);
  const isSelected = selectedFile?.id === file.id;
  const isFolder = file.type === "folder";
  const hasChildren = file.children && file.children.length > 0;

  const handleClick = () => {
    if (isFolder) {
      setIsExpanded(!isExpanded);
    } else {
      onFileSelect(file);
    }
  };

  return (
    <div>
      <Button
        variant="ghost"
        className={cn(
          "w-full justify-start h-8 px-2 font-normal",
          isSelected && "bg-accent text-accent-foreground",
        )}
        style={{ paddingLeft: `${level * 12 + 8}px` }}
        onClick={handleClick}
      >
        <div className="flex items-center gap-1 flex-1">
          {isFolder &&
            hasChildren &&
            (isExpanded ? (
              <ChevronDown className="h-3 w-3" />
            ) : (
              <ChevronRight className="h-3 w-3" />
            ))}
          {!isFolder && <div className="w-3" />}

          {getFileIcon(file.name, isFolder, isExpanded)}

          <span className="truncate text-sm">{file.name}</span>
        </div>
      </Button>

      {isFolder && hasChildren && isExpanded && (
        <div>
          {file.children!.map((child) => (
            <FileTreeItem
              key={child.id}
              file={child}
              selectedFile={selectedFile}
              onFileSelect={onFileSelect}
              level={level + 1}
            />
          ))}
        </div>
      )}
    </div>
  );
}
