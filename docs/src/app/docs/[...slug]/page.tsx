import { Metadata } from "next";
import { notFound } from "next/navigation";
import { DocsFileSystem } from "@/lib/docs/file-system";
import { DocsContent } from "@/components/docs/doc-content";
interface DocsPageProps {
  params: Promise<{ slug: string[] }>;
}

export async function generateMetadata({
  params,
}: DocsPageProps): Promise<Metadata> {
  const { slug } = await params;
  const fileSystem = new DocsFileSystem();
  const doc = await fileSystem.getDocContent(slug);

  if (!doc) {
    return {
      title: "Documentation Not Found | Theo",
    };
  }

  return {
    title: `${doc.frontmatter.title || "Documentation"} | Theo`,
    description: doc.frontmatter.description || "Theo platform documentation",
    keywords: doc.frontmatter.tags || [],
  };
}

export default async function DocsPage({ params }: DocsPageProps) {
  const { slug } = await params;
  const fileSystem = new DocsFileSystem();
  const doc = await fileSystem.getDocContent(slug);

  if (!doc) {
    notFound();
  }

  return (
    <DocsContent
      content={doc.content}
      frontmatter={doc.frontmatter}
      slug={slug}
    />
  );
}
