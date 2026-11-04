/**
 * @jest-environment node
 */
import * as esbuild from "esbuild";
import { THE0_GLOBALS } from "../index";
import { reactGlobalPlugin } from "../esbuild";

describe("window globals shared by the platform and dashboard bundles", () => {
  it("keeps the names that published bundles and platforms already use", () => {
    expect(THE0_GLOBALS).toEqual({
      eventsContext: "__THE0_EVENTS_CONTEXT__",
      react: "__THE0_REACT__",
      reactDom: "__THE0_REACT_DOM__",
      jsxRuntime: "__THE0_REACT_JSX__",
    });
  });

  it("builds bundles that take React from those globals", async () => {
    const result = await esbuild.build({
      stdin: {
        contents: [
          `import * as React from "react";`,
          `import * as ReactDOM from "react-dom";`,
          `import * as Client from "react-dom/client";`,
          `import * as JSX from "react/jsx-runtime";`,
          `import * as DevJSX from "react/jsx-dev-runtime";`,
          `console.log(React, ReactDOM, Client, JSX, DevJSX);`,
        ].join("\n"),
        loader: "js",
      },
      bundle: true,
      format: "esm",
      write: false,
      plugins: [reactGlobalPlugin],
    });
    const code = result.outputFiles[0].text;

    expect(code).toContain(`window.${THE0_GLOBALS.react}`);
    expect(code).toContain(`window.${THE0_GLOBALS.reactDom}`);
    expect(code).toContain(`window.${THE0_GLOBALS.jsxRuntime}`);
  });
});
