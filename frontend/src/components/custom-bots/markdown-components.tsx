import React from 'react';
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter';
import { oneDark } from 'react-syntax-highlighter/dist/esm/styles/prism';
import { Button } from '@/components/ui/button';
import { Copy } from 'lucide-react';

export const MarkdownComponents = {
  code({ node, className, children, ...props }: any) {
    const match = /language-(\w+)/.exec(className || '');
    const language = match ? match[1] : '';

    // Handle code blocks (triple backticks)
    return match ? (
      <div className="my-3 rounded-md overflow-hidden">
        <div className="flex items-center justify-between bg-background/90 px-4 py-1 text-xs text-foreground/70">
          <span>{language || 'Code'}</span>
        </div>
        <SyntaxHighlighter
          style={oneDark}
          language={language || 'javascript'}
          customStyle={{
            margin: 0,
            fontSize: '0.85rem',
            borderRadius: '0 0 0.375rem 0.375rem',
          }}
        >
          {String(children).replace(/\n$/, '')}
        </SyntaxHighlighter>
      </div>
    ) : (
      <code
        className="bg-gray-100 dark:bg-gray-800 rounded px-1 py-0.5 text-sm"
        {...props}
      >
        {children}
      </code>
    );
  },
  a: ({ node, ...props }: any) => (
    <a
      target="_blank"
      rel="noopener noreferrer"
      className="text-blue-500 hover:underline"
      {...props}
    />
  ),
  p: ({ node, children, ...props }: any) => (
    <p className="mb-3 last:mb-0 leading-relaxed" {...props}>
      {children}
    </p>
  ),
  h1: ({ node, children, ...props }: any) => (
    <h1 className="text-xl font-bold mb-3 mt-4" {...props}>
      {children}
    </h1>
  ),
  h2: ({ node, children, ...props }: any) => (
    <h2 className="text-lg font-bold mb-2 mt-4" {...props}>
      {children}
    </h2>
  ),
  h3: ({ node, children, ...props }: any) => (
    <h3 className="text-base font-semibold mb-2 mt-3" {...props}>
      {children}
    </h3>
  ),
  ul: ({ node, children, ...props }: any) => (
    <ul className="pl-6 mb-3 list-disc" {...props}>
      {children}
    </ul>
  ),
  ol: ({ node, children, ...props }: any) => (
    <ol className="pl-6 mb-3 list-decimal" {...props}>
      {children}
    </ol>
  ),
  li: ({ node, children, ...props }: any) => (
    <li className="mb-1" {...props}>
      {children}
    </li>
  ),
  blockquote: ({ node, children, ...props }: any) => (
    <blockquote
      className="border-l-4 border-gray-300 dark:border-gray-700 pl-3 py-1 mb-3 text-foreground/80 italic"
      {...props}
    >
      {children}
    </blockquote>
  ),
  table: ({ node, children, ...props }: any) => (
    <div className="overflow-x-auto mb-3">
      <table className="min-w-full border-collapse text-sm" {...props}>
        {children}
      </table>
    </div>
  ),
  th: ({ node, children, ...props }: any) => (
    <th
      className="border border-gray-300 dark:border-gray-700 px-3 py-2 bg-gray-100 dark:bg-gray-800 font-medium"
      {...props}
    >
      {children}
    </th>
  ),
  td: ({ node, children, ...props }: any) => (
    <td
      className="border border-gray-300 dark:border-gray-700 px-3 py-2"
      {...props}
    >
      {children}
    </td>
  ),
  hr: () => (
    <hr className="border-t border-gray-300 dark:border-gray-700 my-4" />
  ),
};
