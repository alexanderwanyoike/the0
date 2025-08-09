import { DocsFileSystem, DocFrontmatter } from "./file-system";

export interface DocSearchResult {
  slug: string;
  title: string;
  content: string;
  excerpt: string;
  path: string;
  score: number;
}

export class DocsSearchService {
  private searchIndex: Array<{
    slug: string[];
    title: string;
    content: string;
    path: string;
    frontmatter: DocFrontmatter;
  }> = [];

  private static instance: DocsSearchService;

  static getInstance(): DocsSearchService {
    if (!this.instance) {
      this.instance = new DocsSearchService();
    }
    return this.instance;
  }

  async indexDocuments(): Promise<void> {
    const fileSystem = new DocsFileSystem();
    const allDocs = await fileSystem.getAllDocs();

    this.searchIndex = allDocs.map((doc) => ({
      slug: doc.slug,
      title: doc.frontmatter.title || doc.slug.join(" "),
      content: this.cleanContent(doc.content),
      path: doc.slug.join("/"),
      frontmatter: doc.frontmatter,
    }));
  }

  async search(query: string): Promise<DocSearchResult[]> {
    if (!query || query.length < 2) return [];

    const lowerQuery = query.toLowerCase();
    const terms = lowerQuery.split(" ").filter((term) => term.length > 1);

    const results = this.searchIndex
      .map((doc) => ({
        slug: doc.path,
        title: doc.title,
        content: doc.content,
        excerpt: this.generateExcerpt(doc.content, terms),
        path: doc.path,
        score: this.calculateScore(doc, lowerQuery, terms),
      }))
      .filter((doc) => doc.score > 0)
      .sort((a, b) => b.score - a.score)
      .slice(0, 10);

    return results;
  }

  async getSuggestions(query: string): Promise<string[]> {
    if (!query || query.length < 2) return [];

    const lowerQuery = query.toLowerCase();
    const suggestions = new Set<string>();

    this.searchIndex.forEach((doc) => {
      // Title-based suggestions
      if (doc.title.toLowerCase().includes(lowerQuery)) {
        suggestions.add(doc.title);
      }

      // Word-based suggestions from titles
      const titleWords = doc.title.toLowerCase().split(" ");
      titleWords.forEach((word) => {
        if (word.startsWith(lowerQuery) && word.length > lowerQuery.length) {
          suggestions.add(word);
        }
      });

      // Tag-based suggestions
      if (doc.frontmatter.tags) {
        doc.frontmatter.tags.forEach((tag) => {
          if (tag.toLowerCase().includes(lowerQuery)) {
            suggestions.add(tag);
          }
        });
      }
    });

    return Array.from(suggestions).slice(0, 5);
  }

  private calculateScore(
    doc: { title: string; content: string; frontmatter: DocFrontmatter },
    query: string,
    terms: string[],
  ): number {
    let score = 0;
    const lowerTitle = doc.title.toLowerCase();
    const lowerContent = doc.content.toLowerCase();

    // Title exact match gets highest score
    if (lowerTitle.includes(query)) {
      score += 20;
    }

    // Title starts with query
    if (lowerTitle.startsWith(query)) {
      score += 15;
    }

    // Individual term matches in title
    terms.forEach((term) => {
      if (lowerTitle.includes(term)) {
        score += 10;
      }
    });

    // Content matches (lower weight)
    terms.forEach((term) => {
      const contentMatches = (lowerContent.match(new RegExp(term, "g")) || [])
        .length;
      score += contentMatches * 1;
    });

    // Tag matches
    if (doc.frontmatter.tags) {
      doc.frontmatter.tags.forEach((tag) => {
        if (tag.toLowerCase().includes(query)) {
          score += 8;
        }
      });
    }

    // Boost for shorter documents (more focused content)
    if (doc.content.length < 1000) {
      score += 2;
    }

    // Boost for non-draft documents
    if (!doc.frontmatter.draft) {
      score += 1;
    }

    return score;
  }

  private generateExcerpt(content: string, terms: string[]): string {
    const cleanContent = this.cleanContent(content);

    // Find the first occurrence of any search term
    let bestMatch = -1;
    let bestTerm = "";

    for (const term of terms) {
      const index = cleanContent.toLowerCase().indexOf(term);
      if (index !== -1 && (bestMatch === -1 || index < bestMatch)) {
        bestMatch = index;
        bestTerm = term;
      }
    }

    if (bestMatch === -1) {
      // No match found, return beginning of content
      return cleanContent.substring(0, 150).trim() + "...";
    }

    // Create excerpt around the match
    const start = Math.max(0, bestMatch - 75);
    const end = Math.min(cleanContent.length, bestMatch + 75);
    let excerpt = cleanContent.substring(start, end);

    // Add ellipsis if needed
    if (start > 0) excerpt = "..." + excerpt;
    if (end < cleanContent.length) excerpt = excerpt + "...";

    return excerpt.trim();
  }

  private cleanContent(content: string): string {
    return content
      .replace(/#{1,6}\s/g, "") // Remove headers
      .replace(/\*\*(.*?)\*\*/g, "$1") // Remove bold
      .replace(/\*(.*?)\*/g, "$1") // Remove italic
      .replace(/`(.*?)`/g, "$1") // Remove inline code
      .replace(/```[\s\S]*?```/g, "") // Remove code blocks
      .replace(/\[([^\]]+)\]\([^)]+\)/g, "$1") // Remove links, keep text
      .replace(/\n{2,}/g, " ") // Normalize whitespace
      .replace(/\s+/g, " ") // Normalize spaces
      .trim();
  }
}
