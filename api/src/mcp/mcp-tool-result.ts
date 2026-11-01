import { AppError } from "@/common/app-error";

export interface McpToolResult {
  content: Array<{ type: "text"; text: string }>;
  isError?: boolean;
  structuredContent?: Record<string, unknown>;
}

/**
 * The MCP answer to an AppError: a tool result flagged as an error, with the
 * code kept in structured content and repeated as the JSON text, which is
 * what clients without structured content read.
 */
export function toMcpToolError(error: AppError): McpToolResult {
  const structuredContent = {
    error: { code: error.code, message: error.message },
  };
  return {
    content: [
      { type: "text", text: JSON.stringify(structuredContent, null, 2) },
    ],
    isError: true,
    structuredContent,
  };
}

export function toMcpToolResult(data: unknown): McpToolResult {
  return { content: [{ type: "text", text: JSON.stringify(data, null, 2) }] };
}
