import { useEffect, useState, useRef } from "react";
import { useArtifactsStore } from "@/stores/ai-agent/artifactsStore";
import { useChatStore } from "@/stores/ai-agent/chatStore";
import { apiService } from "@/lib/ai-agent/api";
import { ArtifactFile } from "@/types";

const getLanguageFromName = (fileName: string) => {
  const ext = fileName.split(".").pop()?.toLowerCase();
  switch (ext) {
    case "py":
      return "python";
    case "yaml":
    case "yml":
      return "yaml";
    case "json":
      return "json";
    case "md":
      return "markdown";
    case "js":
    case "jsx":
      return "javascript";
    case "ts":
    case "tsx":
      return "typescript";
    default:
      return "plaintext";
  }
};

const createArtifactFile = (name: string): ArtifactFile => ({
  id: name,
  name,
  content: "// Failed to load content",
  language: getLanguageFromName(name),
  type: "file",
});

export const useArtifacts = () => {
  const {
    forceShow,
    files: storeFiles,
    setFiles,
    setActiveFile,
  } = useArtifactsStore();
  const { sessionId } = useChatStore();

  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<Error | null>(null);

  // Use refs to track what we've already fetched to prevent duplicates
  const fetchedSession = useRef<string | null>(null);
  const isFetching = useRef(false);

  // Only fetch when forceShow is true AND sessionId changes AND we haven't fetched this session yet
  useEffect(() => {
    // Clear everything if forceShow is false or no sessionId
    if (!forceShow || !sessionId) {
      if (fetchedSession.current !== null) {
        setFiles([]);
        setActiveFile(null);
        setError(null);
        fetchedSession.current = null;
        isFetching.current = false;
      }
      return;
    }

    // Don't fetch if we're already fetching or already fetched this session
    if (isFetching.current || fetchedSession.current === sessionId) {
      return;
    }

    // Mark as fetching and start fetch
    isFetching.current = true;
    fetchedSession.current = sessionId;

    const fetchArtifacts = async () => {
      setIsLoading(true);
      setError(null);

      try {
        // Get list of artifact names
        const artifactNames = await apiService.getSessionArtifacts(sessionId);

        if (artifactNames.length === 0) {
          setFiles([]);
          return;
        }

        // Fetch content for each file
        const filesWithContent = await Promise.all(
          artifactNames.map(async (name: string) => {
            try {
              const artifact = await apiService.getArtifact(name, sessionId);

              // Handle escaped characters in content
              let content = artifact.content;
              if (typeof content === "string") {
                const hasEscapedNewlines =
                  content.includes("\\n") && !content.includes("\n");
                if (hasEscapedNewlines) {
                  content = content
                    .replace(/\\n/g, "\n")
                    .replace(/\\t/g, "\t")
                    .replace(/\\"/g, '"')
                    .replace(/\\'/g, "'")
                    .replace(/\\\\/g, "\\");
                }
              }

              return {
                id: name,
                name,
                content,
                language: getLanguageFromName(name),
                type: "file" as const,
              };
            } catch (error) {
              console.error(`Failed to fetch content for ${name}:`, error);
              return createArtifactFile(name);
            }
          }),
        );

        setFiles(filesWithContent);
      } catch (err) {
        console.error("Failed to fetch artifacts:", err);
        setError(err as Error);
        setFiles([]);
      } finally {
        setIsLoading(false);
        isFetching.current = false;
      }
    };

    fetchArtifacts();
  }, [forceShow, sessionId]); // Only depend on these two values

  return {
    files: storeFiles,
    isLoading,
    error,
  };
};
