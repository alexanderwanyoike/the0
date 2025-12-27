/**
 * Build utilities for the0 custom bot frontends.
 *
 * Provides a simple way to build frontend bundles with proper React handling.
 */

import * as esbuild from "esbuild";
import { reactGlobalPlugin } from "./esbuild";

export interface BuildOptions {
  /** Entry point file (default: "index.tsx") */
  entryPoint?: string;
  /** Output file (default: "dist/bundle.js") */
  outfile?: string;
  /** Minify output (default: true) */
  minify?: boolean;
  /** Additional esbuild plugins */
  plugins?: esbuild.Plugin[];
}

/**
 * Build a the0 custom bot frontend bundle.
 *
 * @example
 * ```js
 * import { build } from "@alexanderwanyoike/the0-react/build";
 *
 * // Simple usage with defaults
 * await build();
 *
 * // Custom options
 * await build({
 *   entryPoint: "src/Dashboard.tsx",
 *   outfile: "build/bundle.js",
 *   minify: false,
 * });
 * ```
 */
export async function build(options: BuildOptions = {}): Promise<void> {
  const {
    entryPoint = "index.tsx",
    outfile = "dist/bundle.js",
    minify = true,
    plugins = [],
  } = options;

  await esbuild.build({
    entryPoints: [entryPoint],
    bundle: true,
    format: "esm",
    outfile,
    minify,
    plugins: [reactGlobalPlugin, ...plugins],
  });

  console.log(`Build complete: ${outfile}`);
}

// Re-export the plugin for advanced usage
export { reactGlobalPlugin } from "./esbuild";
