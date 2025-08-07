/** @type {import('next').NextConfig} */
import withMarkdoc from "@markdoc/next.js";

const nextConfig = withMarkdoc()({
  reactStrictMode: true,
  pageExtensions: ["md", "mdoc", "js", "jsx", "ts", "tsx"],
  output: "standalone",
});

export default nextConfig;
