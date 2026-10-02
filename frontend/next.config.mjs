/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
  pageExtensions: ["js", "jsx", "ts", "tsx"],
  output: "standalone",
  // Lint runs as a separate non-blocking CI report; with the existing findings, linting inside next build would fail every build.
  eslint: {
    ignoreDuringBuilds: true,
  },
};

export default nextConfig;
