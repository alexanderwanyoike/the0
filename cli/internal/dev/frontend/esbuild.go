// Package frontend serves the custom bot dashboard in `the0 dev --frontend`
// mode. It embeds a tiny HTML shell that mirrors the platform frontend's
// ambient globals (window.__THE0_REACT__ etc.), feeds it events over a local
// WebSocket, and loads the user's bundled TSX on top.
package frontend

import (
	"fmt"

	esbuild "github.com/evanw/esbuild/pkg/api"
)

// reactGlobalPlugin is the Go port of sdk/react/src/esbuild.ts — it rewrites
// `import React from "react"` (and the DOM / jsx-runtime variants) to pull
// from the ambient globals the dev shell sets on window. This lets the
// built bundle share React with the shell, matching how the platform
// frontend loads custom dashboards.
var reactGlobalPlugin = esbuild.Plugin{
	Name: "the0-react-global",
	Setup: func(build esbuild.PluginBuild) {
		type ns struct {
			filter    string
			namespace string
			global    string
		}
		ns3 := []ns{
			{"^react$", "the0-react-global", "window.__THE0_REACT__"},
			{"^react-dom$", "the0-react-dom-global", "window.__THE0_REACT_DOM__"},
			{"^react-dom/client$", "the0-react-dom-client-global", "window.__THE0_REACT_DOM__"},
			{"^react/jsx-runtime$", "the0-react-jsx-runtime-global", "window.__THE0_REACT_JSX__"},
			{"^react/jsx-dev-runtime$", "the0-react-jsx-dev-global", "window.__THE0_REACT_JSX__"},
		}
		for _, n := range ns3 {
			filter, namespace, global := n.filter, n.namespace, n.global
			build.OnResolve(esbuild.OnResolveOptions{Filter: filter}, func(args esbuild.OnResolveArgs) (esbuild.OnResolveResult, error) {
				return esbuild.OnResolveResult{Path: args.Path, Namespace: namespace}, nil
			})
			build.OnLoad(esbuild.OnLoadOptions{Filter: ".*", Namespace: namespace}, func(args esbuild.OnLoadArgs) (esbuild.OnLoadResult, error) {
				body := fmt.Sprintf("module.exports = %s;", global)
				loader := esbuild.LoaderJS
				return esbuild.OnLoadResult{Contents: &body, Loader: loader}, nil
			})
		}
	},
}

// BuildBundle runs esbuild on entry (a TSX file path) and returns the bundled
// ESM output as bytes. Imports of react / react-dom / jsx-runtime are
// rewritten to use ambient globals (see reactGlobalPlugin).
func BuildBundle(entry string, minify bool) ([]byte, error) {
	result := esbuild.Build(esbuild.BuildOptions{
		EntryPoints: []string{entry},
		Bundle:      true,
		Write:       false,
		Format:      esbuild.FormatESModule,
		Loader: map[string]esbuild.Loader{
			".tsx": esbuild.LoaderTSX,
			".ts":  esbuild.LoaderTS,
			".jsx": esbuild.LoaderJSX,
			".js":  esbuild.LoaderJS,
		},
		Plugins:           []esbuild.Plugin{reactGlobalPlugin},
		MinifyWhitespace:  minify,
		MinifyIdentifiers: minify,
		MinifySyntax:      minify,
	})
	if len(result.Errors) > 0 {
		return nil, fmt.Errorf("esbuild: %s", result.Errors[0].Text)
	}
	if len(result.OutputFiles) == 0 {
		return nil, fmt.Errorf("esbuild produced no output")
	}
	return result.OutputFiles[0].Contents, nil
}
