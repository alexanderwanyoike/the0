import { NextResponse } from "next/server";

// Serve React JSX runtime as an ES module for dynamic imports
const moduleCode = `
const JSXRuntime = window.__THE0_REACT_JSX__;
export const jsx = JSXRuntime.jsx;
export const jsxs = JSXRuntime.jsxs;
export const Fragment = JSXRuntime.Fragment;
`;

export async function GET() {
  return new NextResponse(moduleCode, {
    headers: {
      "Content-Type": "application/javascript",
      "Cache-Control": "no-cache, no-store, must-revalidate",
    },
  });
}
