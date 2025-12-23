/**
 * esbuild plugin for the0 custom bot frontends.
 *
 * Transforms React imports to use the host's React instance via window globals.
 * This allows custom bot frontends to share React with the0 platform,
 * avoiding multiple React instances and hooks errors.
 */

import type { Plugin } from "esbuild";

/**
 * esbuild plugin that transforms React imports to use window globals.
 *
 * @example
 * ```js
 * import * as esbuild from "esbuild";
 * import { reactGlobalPlugin } from "@alexanderwanyoike/the0-react/esbuild";
 *
 * await esbuild.build({
 *   entryPoints: ["index.tsx"],
 *   bundle: true,
 *   format: "esm",
 *   outfile: "dist/bundle.js",
 *   plugins: [reactGlobalPlugin],
 * });
 * ```
 */
export const reactGlobalPlugin: Plugin = {
  name: "the0-react-global",
  setup(build) {
    // Resolve react imports to custom namespaces
    build.onResolve({ filter: /^react$/ }, (args) => ({
      path: args.path,
      namespace: "the0-react-global",
    }));

    build.onResolve({ filter: /^react-dom$/ }, (args) => ({
      path: args.path,
      namespace: "the0-react-dom-global",
    }));

    build.onResolve({ filter: /^react-dom\/client$/ }, (args) => ({
      path: args.path,
      namespace: "the0-react-dom-client-global",
    }));

    build.onResolve({ filter: /^react\/jsx-runtime$/ }, (args) => ({
      path: args.path,
      namespace: "the0-react-jsx-runtime-global",
    }));

    build.onResolve({ filter: /^react\/jsx-dev-runtime$/ }, (args) => ({
      path: args.path,
      namespace: "the0-react-jsx-runtime-global",
    }));

    // Load handlers return code that uses window globals
    build.onLoad({ filter: /.*/, namespace: "the0-react-global" }, () => ({
      contents: `module.exports = window.__THE0_REACT__;`,
      loader: "js",
    }));

    build.onLoad({ filter: /.*/, namespace: "the0-react-dom-global" }, () => ({
      contents: `module.exports = window.__THE0_REACT_DOM__;`,
      loader: "js",
    }));

    build.onLoad(
      { filter: /.*/, namespace: "the0-react-dom-client-global" },
      () => ({
        contents: `module.exports = window.__THE0_REACT_DOM__;`,
        loader: "js",
      })
    );

    build.onLoad(
      { filter: /.*/, namespace: "the0-react-jsx-runtime-global" },
      () => ({
        contents: `module.exports = window.__THE0_REACT_JSX__;`,
        loader: "js",
      })
    );
  },
};
