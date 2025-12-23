import { NextResponse } from "next/server";
import React from "react";

/**
 * Serve React as an ES module for dynamic imports.
 * This ensures custom bot frontends use the same React instance as the host.
 *
 * The export list is generated dynamically from the installed React version,
 * so it automatically stays in sync when React is updated.
 */
export async function GET() {
  // Get all React exports dynamically from the installed React package
  const reactExports = Object.keys(React).filter(
    (key) => typeof key === "string" && /^[a-zA-Z]/.test(key),
  );

  const exportStatements = reactExports
    .map((key) => `export const ${key} = React.${key};`)
    .join("\n");

  const moduleCode = `
const React = window.__THE0_REACT__;
if (!React) throw new Error("React not found on window.__THE0_REACT__");

// Dynamically generated exports from React ${React.version || ""}
${exportStatements}

export default React;
`;

  return new NextResponse(moduleCode, {
    headers: {
      "Content-Type": "application/javascript",
      "Cache-Control": "no-cache, no-store, must-revalidate",
    },
  });
}
