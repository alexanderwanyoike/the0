import { describe, it, expect, beforeEach } from "@jest/globals";
import { useArtifactsStore } from "../artifactsStore";
import { ArtifactFile } from "@/types";

const mockFiles: ArtifactFile[] = [
  {
    id: "1",
    name: "main.py",
    content: 'print("hello")',
    language: "python",
    type: "file",
  },
  {
    id: "2",
    name: "config.yaml",
    content: "debug: true",
    language: "yaml",
    type: "file",
  },
];

describe("artifactsStore", () => {
  beforeEach(() => {
    useArtifactsStore.setState({
      files: [],
      activeFile: null,
      selectedFiles: [],
      forceShow: false,
    });
  });

  it("should set files correctly", () => {
    const { setFiles } = useArtifactsStore.getState();

    setFiles(mockFiles);

    const { files } = useArtifactsStore.getState();
    expect(files).toEqual(mockFiles);
  });

  it("should set active file correctly", () => {
    const { setActiveFile } = useArtifactsStore.getState();

    setActiveFile(mockFiles[0]);

    const { activeFile } = useArtifactsStore.getState();
    expect(activeFile).toEqual(mockFiles[0]);
  });

  it("should update file content correctly", () => {
    const { setFiles, updateFile } = useArtifactsStore.getState();

    setFiles(mockFiles);
    updateFile("1", 'print("updated")');

    const { files } = useArtifactsStore.getState();
    expect(files[0].content).toBe('print("updated")');
  });

  it("should delete file correctly", () => {
    const { setFiles, deleteFile } = useArtifactsStore.getState();

    setFiles(mockFiles);
    deleteFile("1");

    const { files } = useArtifactsStore.getState();
    expect(files).toHaveLength(1);
    expect(files[0].id).toBe("2");
  });

  it("should clear active file when deleted file is active", () => {
    const { setFiles, setActiveFile, deleteFile } =
      useArtifactsStore.getState();

    setFiles(mockFiles);
    setActiveFile(mockFiles[0]);
    deleteFile("1");

    const { activeFile } = useArtifactsStore.getState();
    expect(activeFile).toBeNull();
  });

  it("should preserve active file when different file is deleted", () => {
    const { setFiles, setActiveFile, deleteFile } =
      useArtifactsStore.getState();

    setFiles(mockFiles);
    setActiveFile(mockFiles[0]);
    deleteFile("2");

    const { activeFile } = useArtifactsStore.getState();
    expect(activeFile).toEqual(mockFiles[0]);
  });

  it("should manage selected files correctly", () => {
    const { addSelectedFile, removeSelectedFile, clearSelectedFiles } =
      useArtifactsStore.getState();

    addSelectedFile("1");
    addSelectedFile("2");

    let { selectedFiles } = useArtifactsStore.getState();
    expect(selectedFiles).toEqual(["1", "2"]);

    removeSelectedFile("1");
    selectedFiles = useArtifactsStore.getState().selectedFiles;
    expect(selectedFiles).toEqual(["2"]);

    clearSelectedFiles();
    selectedFiles = useArtifactsStore.getState().selectedFiles;
    expect(selectedFiles).toEqual([]);
  });

  it("should not add duplicate selected files", () => {
    const { addSelectedFile } = useArtifactsStore.getState();

    addSelectedFile("1");
    addSelectedFile("1");

    const { selectedFiles } = useArtifactsStore.getState();
    expect(selectedFiles).toEqual(["1"]);
  });

  it("should handle forceShow flag", () => {
    const { setForceShow } = useArtifactsStore.getState();

    setForceShow(true);

    let { forceShow } = useArtifactsStore.getState();
    expect(forceShow).toBe(true);

    setForceShow(false);
    forceShow = useArtifactsStore.getState().forceShow;
    expect(forceShow).toBe(false);
  });
});
