import { DocsLayout } from "@/components/docs/docs-layout";
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

export default async function HomePage() {
  const fileSystem = new DocsFileSystem();
  const doc = await fileSystem.getDocContent(["index"]);

  // If no index.md found, show empty state
  const content = doc ? (
    <DocsContent
      content={doc.content}
      frontmatter={doc.frontmatter}
      slug={["index"]}
    />
  ) : (
    <div className="space-y-8">
      <div className="text-center space-y-4">
        <BookOpen className="h-16 w-16 text-muted-foreground mx-auto" />
        <h1 className="text-4xl font-bold tracking-tight">Documentation</h1>
        <p className="text-xl text-muted-foreground max-w-2xl mx-auto">
          Welcome to the comprehensive documentation for the0 platform. This
          documentation covers everything you need to know about algorithmic
          trading with the0.
        </p>
      </div>
      <Card className="max-w-2xl mx-auto">
        <CardHeader>
          <CardTitle>Getting Started</CardTitle>
          <CardDescription>
            Explore the platform and build your first trading bot
          </CardDescription>
        </CardHeader>
        <CardContent>
          <p className="text-sm text-muted-foreground">
            Use the navigation menu to explore different sections and learn how
            to create and deploy trading bots.
          </p>
        </CardContent>
      </Card>
    </div>
  );

  return <DocsLayout currentPath={[]}>{content}</DocsLayout>;
}
