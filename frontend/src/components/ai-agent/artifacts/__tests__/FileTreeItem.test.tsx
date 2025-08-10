import { render, screen, fireEvent } from "@testing-library/react";
import { describe, it, expect } from "@jest/globals";
import { jest } from "@jest/globals";
import { FileTreeItem } from "../FileTreeItem";
import { ArtifactFile } from "@/types";

const mockFile: ArtifactFile = {
  id: "1",
  name: "test.py",
  content: 'print("hello")',
  language: "python",
  type: "file",
};

const mockFolder: ArtifactFile = {
  id: "2",
  name: "src",
  content: "",
  language: "",
  type: "folder",
  children: [mockFile],
};

describe("FileTreeItem", () => {
  it("renders file item correctly", () => {
    const onFileSelect = jest.fn();

    render(
      <FileTreeItem
        file={mockFile}
        selectedFile={null}
        onFileSelect={onFileSelect}
        level={0}
      />,
    );

    expect(screen.getByText("test.py")).toBeInTheDocument();
  });

  it("calls onFileSelect when file is clicked", () => {
    const onFileSelect = jest.fn();

    render(
      <FileTreeItem
        file={mockFile}
        selectedFile={null}
        onFileSelect={onFileSelect}
        level={0}
      />,
    );

    fireEvent.click(screen.getByText("test.py"));
    expect(onFileSelect).toHaveBeenCalledWith(mockFile);
  });

  it("shows selected state when file is selected", () => {
    const onFileSelect = jest.fn();

    render(
      <FileTreeItem
        file={mockFile}
        selectedFile={mockFile}
        onFileSelect={onFileSelect}
        level={0}
      />,
    );

    const button = screen.getByRole("button");
    expect(button).toHaveClass("bg-accent");
  });

  it("renders folder with expand/collapse functionality", () => {
    const onFileSelect = jest.fn();

    render(
      <FileTreeItem
        file={mockFolder}
        selectedFile={null}
        onFileSelect={onFileSelect}
        level={0}
      />,
    );

    expect(screen.getByText("src")).toBeInTheDocument();

    // Initially collapsed
    expect(screen.queryByText("test.py")).not.toBeInTheDocument();

    // Click to expand
    fireEvent.click(screen.getByText("src"));

    // Should now show children
    expect(screen.getByText("test.py")).toBeInTheDocument();
  });

  it("does not call onFileSelect when folder is clicked", () => {
    const onFileSelect = jest.fn();

    render(
      <FileTreeItem
        file={mockFolder}
        selectedFile={null}
        onFileSelect={onFileSelect}
        level={0}
      />,
    );

    fireEvent.click(screen.getByText("src"));
    expect(onFileSelect).not.toHaveBeenCalled();
  });
});
