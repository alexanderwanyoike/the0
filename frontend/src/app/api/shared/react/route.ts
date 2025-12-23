import { NextResponse } from "next/server";

/**
 * Serve React as an ES module for dynamic imports.
 * This ensures custom bot frontends use the same React instance as the host.
 *
 * Note: We explicitly list React exports because Object.keys() on the server
 * doesn't reliably enumerate all React APIs. When React adds new exports,
 * add them to the REACT_EXPORTS array below.
 * 
 * TODO: This feels brittle but dynamically generating the module code is tricky since
 * some members are not immediately available on the React object.
 * We should revisit this approach 
 */

// All public React APIs - update this list when React version changes
const REACT_EXPORTS = [
  // Components
  "Children",
  "Component",
  "Fragment",
  "Profiler",
  "PureComponent",
  "StrictMode",
  "Suspense",
  // React 18+ APIs
  "startTransition",
  "useTransition",
  "useDeferredValue",
  "useId",
  "useSyncExternalStore",
  "useInsertionEffect",
  // React 19 APIs
  "useOptimistic",
  "useActionState",
  "use",
  "cache",
  // Core hooks
  "useCallback",
  "useContext",
  "useDebugValue",
  "useEffect",
  "useImperativeHandle",
  "useLayoutEffect",
  "useMemo",
  "useReducer",
  "useRef",
  "useState",
  // Element APIs
  "cloneElement",
  "createContext",
  "createElement",
  "createFactory",
  "createRef",
  "forwardRef",
  "isValidElement",
  "lazy",
  "memo",
  // Other
  "version",
  "unstable_act",
];

export async function GET() {
  const exportStatements = REACT_EXPORTS.map(
    (key) => `export const ${key} = React.${key};`,
  ).join("\n");

  const moduleCode = `
const React = window.__THE0_REACT__;
if (!React) throw new Error("React not found on window.__THE0_REACT__");

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
