'use client';

import React from 'react';
import ReactMarkdown from 'react-markdown';
import { DocFrontmatter } from '@/lib/docs/file-system';
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter';
import {
  vscDarkPlus,
  vs,
} from 'react-syntax-highlighter/dist/esm/styles/prism';
import remarkGfm from 'remark-gfm';
import { useTheme } from 'next-themes';

interface DocsContentProps {
  content: string;
  frontmatter: DocFrontmatter;
  slug: string[];
}

export const DocsContent: React.FC<DocsContentProps> = ({
  content,
  frontmatter,
  slug,
}) => {
  const { resolvedTheme } = useTheme();
  // Clean up Markdoc-specific tags that won't render properly
  const cleanContent = content
    .replace(/\{%\s*button[^%]*%\}[\s\S]*?\{%\s*\/button\s*%\}/gi, '')
    .replace(/\{%\s*alert[^%]*%\}[\s\S]*?\{%\s*\/alert\s*%\}/gi, '')
    .replace(/\{%\s*card[^%]*%\}[\s\S]*?\{%\s*\/card\s*%\}/gi, '')
    .replace(/\{%\s*[^%]*%\}/gi, '');

  // Helper function to resolve relative links
  const resolveLink = (href: string): string => {
    // If it's an absolute URL, return as is
    if (
      href.startsWith('http://') ||
      href.startsWith('https://') ||
      href.startsWith('#')
    ) {
      return href;
    }

    // Handle relative links
    if (href.startsWith('./')) {
      // Remove leading ./
      const relativePath = href.slice(2);
      // Get current directory path
      const currentPath = slug.slice(0, -1); // Remove last segment (current file)
      // Construct the full path
      const fullPath =
        currentPath.length > 0
          ? `/docs/${currentPath.join('/')}/${relativePath}`
          : `/docs/${relativePath}`;
      return fullPath;
    }

    // Handle absolute paths from root
    if (href.startsWith('/')) {
      return href;
    }

    // Handle relative paths without ./
    const currentPath = slug.slice(0, -1); // Remove last segment (current file)
    const fullPath =
      currentPath.length > 0
        ? `/docs/${currentPath.join('/')}/${href}`
        : `/docs/${href}`;
    return fullPath;
  };

  return (
    <div className="max-w-none">
      {frontmatter.title && (
        <header className="mb-8 pb-6 border-b border-border">
          <h1 className="text-3xl font-bold mb-2">{frontmatter.title}</h1>
          {frontmatter.description && (
            <p className="text-lg text-muted-foreground">
              {frontmatter.description}
            </p>
          )}
          {frontmatter.tags && frontmatter.tags.length > 0 && (
            <div className="flex flex-wrap gap-2 mt-4">
              {frontmatter.tags.map((tag) => (
                <span
                  key={tag}
                  className="px-2 py-1 text-xs bg-secondary text-secondary-foreground rounded-md"
                >
                  {tag}
                </span>
              ))}
            </div>
          )}
        </header>
      )}
      <div className="prose prose-slate dark:prose-invert max-w-none prose-headings:scroll-mt-20 prose-headings:font-semibold prose-h1:text-3xl prose-h2:text-2xl prose-h3:text-xl prose-h4:text-lg prose-p:leading-7 prose-pre:bg-slate-900 prose-pre:border prose-code:text-sm">
        <ReactMarkdown
          remarkPlugins={[remarkGfm]}
          components={{
            code({ className, children, ...props }: any) {
              const match = /language-(\w+)/.exec(className || '');
              const language = match ? match[1] : '';

              if (language) {
                return (
                  <SyntaxHighlighter
                    style={resolvedTheme === 'dark' ? vscDarkPlus : vs}
                    language={language}
                    PreTag="div"
                    className="rounded-lg border"
                    {...props}
                  >
                    {String(children).replace(/\n$/, '')}
                  </SyntaxHighlighter>
                );
              }

              return (
                <code
                  className="bg-muted px-1.5 py-0.5 rounded text-sm"
                  {...props}
                >
                  {children}
                </code>
              );
            },
            table({ children }) {
              return (
                <div className="my-6 overflow-x-auto rounded-lg border border-border">
                  <table className="w-full border-collapse">{children}</table>
                </div>
              );
            },
            thead({ children }) {
              return <thead className="bg-muted/50">{children}</thead>;
            },
            th({ children }) {
              return (
                <th className="border-b border-border px-4 py-3 text-left text-sm font-semibold text-foreground">
                  {children}
                </th>
              );
            },
            td({ children }) {
              return (
                <td className="border-b border-border px-4 py-3 text-sm text-foreground">
                  {children}
                </td>
              );
            },
            blockquote({ children }) {
              return (
                <blockquote className="border-l-4 border-blue-500 bg-blue-50 dark:bg-blue-950/30 pl-4 py-2 my-4 italic">
                  {children}
                </blockquote>
              );
            },
            h1({ children }) {
              return (
                <h1 className="text-3xl font-bold mt-8 mb-4 first:mt-0">
                  {children}
                </h1>
              );
            },
            h2({ children }) {
              return (
                <h2 className="text-2xl font-semibold mt-8 mb-4">{children}</h2>
              );
            },
            h3({ children }) {
              return (
                <h3 className="text-xl font-semibold mt-6 mb-3">{children}</h3>
              );
            },
            p({ children }) {
              return <p className="mb-4 leading-7">{children}</p>;
            },
            ul({ children }) {
              return (
                <ul className="list-disc pl-6 mb-4 space-y-2">{children}</ul>
              );
            },
            ol({ children }) {
              return (
                <ol className="list-decimal pl-6 mb-4 space-y-2">{children}</ol>
              );
            },
            a({ href, children }) {
              if (!href) return <span>{children}</span>;

              const resolvedHref = resolveLink(href);

              // If it's an external link, use regular anchor
              if (href.startsWith('http://') || href.startsWith('https://')) {
                return (
                  <a
                    href={resolvedHref}
                    target="_blank"
                    rel="noopener noreferrer"
                    className="!text-primary hover:!text-primary/80 !no-underline"
                    style={{
                      color: 'hsl(var(--primary))',
                      textDecoration: 'none',
                    }}
                  >
                    {children}
                  </a>
                );
              }

              // If it's a hash link, use regular anchor
              if (href.startsWith('#')) {
                return (
                  <a
                    href={resolvedHref}
                    className="!text-primary hover:!text-primary/80 !no-underline"
                    style={{
                      color: 'hsl(var(--primary))',
                      textDecoration: 'none',
                    }}
                  >
                    {children}
                  </a>
                );
              }

              // Use regular anchor for internal links
              return (
                <a
                  href={resolvedHref}
                  className="!text-primary hover:!text-primary/80 !no-underline"
                >
                  {children}
                </a>
              );
            },
          }}
        >
          {cleanContent}
        </ReactMarkdown>
      </div>
    </div>
  );
};
