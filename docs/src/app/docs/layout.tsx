import React from "react";
import { DocsLayout } from "@/components/docs/docs-layout";
import { Metadata } from "next";

export const metadata: Metadata = {
  title: "Documentation | Theo",
  description: "Comprehensive documentation for the Theo trading bot platform",
  keywords: ["trading bots", "documentation", "API", "guide", "theo"],
};

interface DocsLayoutPageProps {
  children: React.ReactNode;
}

export default function DocsLayoutPage({ children }: DocsLayoutPageProps) {
  return <DocsLayout>{children}</DocsLayout>;
}
