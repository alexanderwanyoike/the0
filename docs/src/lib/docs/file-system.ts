import fs from "fs/promises";
import path from "path";
import matter from "gray-matter";

export interface DocItem {
  slug: string;
  title: string;
  path: string;
  children?: DocItem[];
  type: "file" | "folder";
  order?: number;
  description?: string;
}

export interface DocFrontmatter {
  title?: string;
  description?: string;
  tags?: string[];
  order?: number;
  draft?: boolean;
}

export interface DocContent {
  content: string;
  frontmatter: DocFrontmatter;
}

export class DocsFileSystem {
  private docsPath = path.join(process.cwd(), "src/docs");

  async getNavigationTree(): Promise<DocItem[]> {
    try {
      // Check if docs directory exists
      await fs.access(this.docsPath);
    } catch {
      // Create docs directory if it doesn't exist
      await fs.mkdir(this.docsPath, { recursive: true });
      return [];
    }

    const buildTree = async (
      dirPath: string,
      basePath: string = "",
    ): Promise<DocItem[]> => {
      const items = await fs.readdir(dirPath, { withFileTypes: true });
      const result: DocItem[] = [];

      for (const item of items) {
        const itemPath = path.join(dirPath, item.name);
        const relativePath = basePath ? `${basePath}/${item.name}` : item.name;

        if (item.isDirectory()) {
          const children = await buildTree(itemPath, relativePath);
          const slug = item.name;
          const title = this.formatTitle(item.name);

          // Try to get order from index file in the directory
          const directoryOrder = await this.getDirectoryOrder(itemPath);

          result.push({
            slug,
            title,
            path: relativePath,
            type: "folder",
            children: children.length > 0 ? children : undefined,
            order: directoryOrder,
          });
        } else if (item.name.endsWith(".md") || item.name.endsWith(".mdoc")) {
          const slug = item.name.replace(/\.(md|mdoc)$/, "");

          // Skip index files in navigation
          if (slug === "index") {
            continue;
          }

          const frontmatter = await this.getFrontmatter(itemPath);
          const title = frontmatter.title || this.formatTitle(slug);

          result.push({
            slug,
            title,
            path: relativePath.replace(/\.(md|mdoc)$/, ""),
            type: "file",
            order: frontmatter.order,
            description: frontmatter.description,
          });
        }
      }

      // Sort items: by order first, then folders before files, then by name
      return result.sort((a, b) => {
        // Sort by order if present (highest priority)
        if (a.order !== undefined && b.order !== undefined) {
          return a.order - b.order;
        }
        if (a.order !== undefined) return -1;
        if (b.order !== undefined) return 1;

        // Then sort by type (folders before files)
        if (a.type !== b.type) {
          return a.type === "folder" ? -1 : 1;
        }

        // Finally sort by title alphabetically
        return a.title.localeCompare(b.title);
      });
    };

    return buildTree(this.docsPath);
  }

  async getDocContent(slug: string[]): Promise<DocContent | null> {
    try {
      // Try both .md and .mdoc extensions
      let filePath = path.join(this.docsPath, ...slug) + ".md";

      try {
        await fs.access(filePath);
      } catch {
        filePath = path.join(this.docsPath, ...slug) + ".mdoc";
        try {
          await fs.access(filePath);
        } catch {
          return null;
        }
      }

      const fileContent = await fs.readFile(filePath, "utf8");
      const { content, data } = matter(fileContent);

      return {
        content,
        frontmatter: data as DocFrontmatter,
      };
    } catch (error) {
      console.error("Error reading doc content:", error);
      return null;
    }
  }

  private async getFrontmatter(filePath: string): Promise<DocFrontmatter> {
    try {
      const fileContent = await fs.readFile(filePath, "utf8");
      const { data } = matter(fileContent);
      return data as DocFrontmatter;
    } catch (error) {
      console.error("Error reading frontmatter:", error);
      return {};
    }
  }

  private formatTitle(slug: string): string {
    return slug
      .replace(/[-_]/g, " ")
      .replace(/\b\w/g, (char) => char.toUpperCase());
  }

  async getAllDocs(): Promise<
    Array<{ slug: string[]; content: string; frontmatter: DocFrontmatter }>
  > {
    const navigationTree = await this.getNavigationTree();
    const docs: Array<{
      slug: string[];
      content: string;
      frontmatter: DocFrontmatter;
    }> = [];

    const extractDocs = async (
      items: DocItem[],
      currentPath: string[] = [],
    ): Promise<void> => {
      for (const item of items) {
        const itemPath = [...currentPath, item.slug];

        if (item.type === "file") {
          const doc = await this.getDocContent(itemPath);
          if (doc) {
            docs.push({
              slug: itemPath,
              content: doc.content,
              frontmatter: doc.frontmatter,
            });
          }
        } else if (item.children) {
          await extractDocs(item.children, itemPath);
        }
      }
    };

    await extractDocs(navigationTree);
    return docs;
  }

  private async getDirectoryOrder(
    dirPath: string,
  ): Promise<number | undefined> {
    // Try to read order from index.md or README.md in the directory
    const indexFiles = ["index.md", "README.md", "index.mdoc"];

    for (const indexFile of indexFiles) {
      const indexPath = path.join(dirPath, indexFile);
      try {
        await fs.access(indexPath);
        const frontmatter = await this.getFrontmatter(indexPath);
        if (frontmatter.order !== undefined) {
          return frontmatter.order;
        }
      } catch {
        // File doesn't exist, continue to next
        continue;
      }
    }

    // No order found in index files
    return undefined;
  }
}
