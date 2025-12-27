#!/usr/bin/env node
/**
 * CLI for building the0 custom bot frontends.
 *
 * Usage:
 *   the0-build                    # Build with defaults (index.tsx -> dist/bundle.js)
 *   the0-build src/App.tsx        # Custom entry point
 *   the0-build -o build/out.js    # Custom output
 */

import { build } from "./build";
import { existsSync } from "fs";

async function main() {
  const args = process.argv.slice(2);

  let entryPoint = "index.tsx";
  let outfile = "dist/bundle.js";
  let minify = true;

  // Parse arguments
  for (let i = 0; i < args.length; i++) {
    const arg = args[i];

    if (arg === "-o" || arg === "--outfile") {
      outfile = args[++i];
    } else if (arg === "--no-minify") {
      minify = false;
    } else if (arg === "-h" || arg === "--help") {
      console.log(`
the0-build - Build the0 custom bot frontends

Usage:
  the0-build [entry] [options]

Arguments:
  entry              Entry point file (default: index.tsx)

Options:
  -o, --outfile      Output file (default: dist/bundle.js)
  --no-minify        Disable minification
  -h, --help         Show this help message

Examples:
  the0-build                        # Build index.tsx to dist/bundle.js
  the0-build src/Dashboard.tsx      # Custom entry point
  the0-build -o build/bundle.js     # Custom output location
`);
      process.exit(0);
    } else if (!arg.startsWith("-")) {
      entryPoint = arg;
    }
  }

  // Check entry point exists
  if (!existsSync(entryPoint)) {
    console.error(`Error: Entry point not found: ${entryPoint}`);
    process.exit(1);
  }

  try {
    await build({ entryPoint, outfile, minify });
  } catch (error) {
    console.error("Build failed:", error);
    process.exit(1);
  }
}

main();
