import { Metadata } from "next";
import { DocsFileSystem } from "@/lib/docs/file-system";
import { DocsContent } from "@/components/docs/doc-content";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { BookOpen } from "lucide-react";

export async function generateMetadata(): Promise<Metadata> {
  const fileSystem = new DocsFileSystem();
  const doc = await fileSystem.getDocContent(["index"]);

  if (!doc) {
    return {
      title: "Documentation | Theo",
      description: "Theo platform documentation",
    };
  }

  return {
    title: `${doc.frontmatter.title || "Documentation"} | Theo`,
    description: doc.frontmatter.description || "Theo platform documentation",
    keywords: doc.frontmatter.tags || [],
  };
}

export default async function DocsHomePage() {
  const fileSystem = new DocsFileSystem();
  const doc = await fileSystem.getDocContent(["index"]);

  // If no index.md found, show empty state
  if (!doc) {
    return (
      <div className="space-y-8">
        <div className="text-center space-y-4">
          <BookOpen className="h-16 w-16 text-muted-foreground mx-auto" />
          <h1 className="text-4xl font-bold tracking-tight">Documentation</h1>
          <p className="text-xl text-muted-foreground max-w-2xl mx-auto">
            Documentation is being prepared. Please check back soon.
          </p>
        </div>
        <Card className="max-w-2xl mx-auto">
          <CardHeader>
            <CardTitle>Getting Started</CardTitle>
            <CardDescription>
              While we prepare the documentation, you can explore the platform
            </CardDescription>
          </CardHeader>
          <CardContent>
            <p className="text-sm text-muted-foreground">
              Use the navigation menu to explore available sections as they
              become ready.
            </p>
          </CardContent>
        </Card>
      </div>
    );
  }

  return (
    <DocsContent
      content={doc.content}
      frontmatter={doc.frontmatter}
      slug={["index"]}
    />
  );
}
