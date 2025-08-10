/** @type {import('next').NextConfig} */
import withMarkdoc from "@markdoc/next.js";

const nextConfig = withMarkdoc()({
  reactStrictMode: true,
  pageExtensions: ["md", "mdoc", "js", "jsx", "ts", "tsx"],
  trailingSlash: true,
  images: {
    unoptimized: true
  }
});

export default nextConfig;
