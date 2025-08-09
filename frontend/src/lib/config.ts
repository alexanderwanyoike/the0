// Configuration for external services
// Uses NEXT_PUBLIC_ for build-time embedding and runtime flexibility

export const getDocsUrl = (): string => {
  // Primary: Use NEXT_PUBLIC_DOCS_URL (embedded at build time)
  // Fallback: Use /docs for backward compatibility
  return process.env.NEXT_PUBLIC_DOCS_URL || '/docs';
};

export const config = {
  docsUrl: getDocsUrl(),
  appName: process.env.NEXT_PUBLIC_APP_NAME || 'the0',
  appVersion: process.env.NEXT_PUBLIC_APP_VERSION || '1.0.0',
};