/**
 * The contract between the0 platform (the host page) and custom dashboard
 * bundles. Bundles built with older SDK versions keep running on newer
 * platforms, so every change here must be additive.
 */

/**
 * Window globals the platform sets before it loads a dashboard bundle.
 * Bundles read React from them so that host and dashboard share one React.
 */
export const THE0_GLOBALS = {
  eventsContext: "__THE0_EVENTS_CONTEXT__",
  react: "__THE0_REACT__",
  reactDom: "__THE0_REACT_DOM__",
  jsxRuntime: "__THE0_REACT_JSX__",
} as const;
