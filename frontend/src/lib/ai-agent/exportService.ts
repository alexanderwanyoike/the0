import { ArtifactFile } from "@/types";
import {
  generateArtifactZip,
  generateExportFilename,
} from "@/lib/zipGenerator";

// Extend window type to include pywebview API
declare global {
  interface Window {
    pywebview?: {
      api?: {
        save_file?: (
          content: string,
          filename: string,
          filters?: string,
        ) => Promise<string>;
        save_file_dialog?: (
          content: string,
          filename: string,
        ) => Promise<string>;
        get_save_filename?: (filename: string) => Promise<string>;
      };
    };
  }
}

export interface ExportOptions {
  filename?: string;
  showProgress?: boolean;
  format?: "zip" | "json";
}

export interface ExportResult {
  success: boolean;
  filename?: string;
  error?: string;
  size?: number;
}

/**
 * Detects if the application is running in a pywebview environment
 */
export function isRunningInPywebview(): boolean {
  return !!(
    window.pywebview?.api ||
    navigator.userAgent.includes("pywebview") ||
    // Additional checks for pywebview environment
    (!window.history?.pushState && !window.opener)
  );
}

/**
 * Exports artifacts as a ZIP file, with pywebview-aware handling
 */
export async function exportArtifacts(
  files: ArtifactFile[],
  options: ExportOptions = {},
): Promise<ExportResult> {
  try {
    if (files.length === 0) {
      return {
        success: false,
        error: "No files to export",
      };
    }

    const filename = options.filename || generateExportFilename();
    const format = options.format || "zip";

    if (format === "zip") {
      return await exportAsZip(files, filename);
    } else {
      return await exportAsJson(files, filename);
    }
  } catch (error) {
    console.error("Export failed:", error);
    return {
      success: false,
      error: error instanceof Error ? error.message : "Unknown export error",
    };
  }
}

/**
 * Exports files as a ZIP archive
 */
async function exportAsZip(
  files: ArtifactFile[],
  filename: string,
): Promise<ExportResult> {
  const blob = await generateArtifactZip(files, filename.replace(".zip", ""));

  if (isRunningInPywebview() && window.pywebview?.api?.save_file) {
    return await saveFileInPywebview(blob, filename);
  } else {
    return saveBlobInBrowser(blob, filename);
  }
}

/**
 * Exports files as JSON (for debugging or alternative format)
 */
async function exportAsJson(
  files: ArtifactFile[],
  filename: string,
): Promise<ExportResult> {
  const exportData = {
    exportDate: new Date().toISOString(),
    generator: "the0-ai-frontend",
    format: "json",
    files: files
      .filter((f) => f.type === "file")
      .map((f) => ({
        name: f.name,
        content: f.content,
        language: f.language,
        id: f.id,
      })),
  };

  const jsonContent = JSON.stringify(exportData, null, 2);
  const blob = new Blob([jsonContent], { type: "application/json" });
  const jsonFilename = filename.replace(/\.(zip|json)$/, "") + ".json";

  if (isRunningInPywebview() && window.pywebview?.api?.save_file) {
    return await saveFileInPywebview(blob, jsonFilename);
  } else {
    return saveBlobInBrowser(blob, jsonFilename);
  }
}

/**
 * Saves a file using pywebview's save_file API
 */
async function saveFileInPywebview(
  blob: Blob,
  filename: string,
): Promise<ExportResult> {
  try {
    // Convert blob to base64 for pywebview
    const arrayBuffer = await blob.arrayBuffer();
    const base64 = btoa(String.fromCharCode(...new Uint8Array(arrayBuffer)));

    // Try to save using pywebview API
    if (window.pywebview?.api?.save_file) {
      const savedPath = await window.pywebview.api.save_file(base64, filename);
      return {
        success: true,
        filename: savedPath || filename,
        size: blob.size,
      };
    } else {
      // Fallback to browser download if pywebview API is not available
      return saveBlobInBrowser(blob, filename);
    }
  } catch (error) {
    console.error(
      "PyWebview save failed, falling back to browser download:",
      error,
    );
    return saveBlobInBrowser(blob, filename);
  }
}

/**
 * Saves a blob using standard browser download
 */
function saveBlobInBrowser(blob: Blob, filename: string): ExportResult {
  try {
    const url = window.URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    window.URL.revokeObjectURL(url);

    return {
      success: true,
      filename,
      size: blob.size,
    };
  } catch (error) {
    return {
      success: false,
      error: error instanceof Error ? error.message : "Browser download failed",
    };
  }
}

/**
 * Gets file size information for display purposes
 */
export function getExportSizeInfo(files: ArtifactFile[]): {
  fileCount: number;
  totalSize: number;
  estimatedZipSize: number;
} {
  const fileFiles = files.filter((f) => f.type === "file");
  const totalSize = fileFiles.reduce(
    (total, file) => total + (file.content?.length || 0),
    0,
  );
  const estimatedZipSize = Math.round(totalSize * 0.4 + 1024); // Rough ZIP compression estimate

  return {
    fileCount: fileFiles.length,
    totalSize,
    estimatedZipSize,
  };
}
