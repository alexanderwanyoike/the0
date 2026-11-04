// Builds a dashboard the way `the0-build` does, once with the published
// the0-react the frontend installs and once with sdk/react's source, and
// prints both bundles as CommonJS. It runs in its own Node process because
// esbuild refuses to run inside jsdom.
import path from "node:path";
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";
import * as esbuild from "esbuild";
import { reactGlobalPlugin as publishedPlugin } from "the0-react/esbuild";

const require = createRequire(import.meta.url);
const repo = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../../..",
);
const sdkSource = path.join(repo, "sdk/react/src");
const [entry] = process.argv.slice(2);

async function sourcePlugin() {
  const { outputFiles } = await esbuild.build({
    entryPoints: [path.join(sdkSource, "esbuild.ts")],
    bundle: true,
    platform: "node",
    format: "esm",
    write: false,
  });
  const code = Buffer.from(outputFiles[0].text).toString("base64");
  const mod = await import(`data:text/javascript;base64,${code}`);
  return mod.reactGlobalPlugin;
}

function resolveSdkTo(file) {
  return {
    name: "resolve-the0-react",
    setup(build) {
      build.onResolve({ filter: /^the0-react$/ }, () => ({ path: file }));
    },
  };
}

async function bundle(sdkEntry, plugin) {
  const { outputFiles } = await esbuild.build({
    entryPoints: [entry],
    bundle: true,
    format: "esm",
    minify: true,
    write: false,
    plugins: [resolveSdkTo(sdkEntry), plugin],
  });
  const { code } = await esbuild.transform(outputFiles[0].text, {
    format: "cjs",
  });
  return code;
}

const published = await bundle(require.resolve("the0-react"), publishedPlugin);
const source = await bundle(
  path.join(sdkSource, "index.ts"),
  await sourcePlugin(),
);
process.stdout.write(JSON.stringify({ published, source }));
