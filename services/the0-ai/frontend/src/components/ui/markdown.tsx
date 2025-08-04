import ReactMarkdown, { Components } from 'react-markdown'
import remarkGfm from 'remark-gfm'
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter'
import { oneDark, oneLight } from 'react-syntax-highlighter/dist/esm/styles/prism'
import { useTheme } from 'next-themes'
import { cn } from '@/lib/utils'

interface MarkdownProps {
  children: string
  className?: string
}

export function Markdown({ children, className }: MarkdownProps) {
  const { theme } = useTheme()

  const components: Components = {
    // Code blocks
    code({ node, className, children, ...props }) {
      const match = /language-(\w+)/.exec(className || '')
      const language = match ? match[1] : ''
      const isInline = !language

      if (!isInline && language) {
        return (
          <SyntaxHighlighter
            style={theme === 'dark' ? oneDark : (oneLight as any)}
            language={language}
            PreTag="div"
            className="rounded-md text-sm"
          >
            {String(children).replace(/\n$/, '')}
          </SyntaxHighlighter>
        )
      }

      return (
        <code
          className={cn(
            'relative rounded bg-muted px-[0.3rem] py-[0.2rem] font-mono text-sm font-semibold',
            className
          )}
          {...props}
        >
          {children}
        </code>
      )
    },

    // Headings
    h1: ({ className, ...props }) => (
      <h1
        className={cn(
          'scroll-m-20 text-xl font-extrabold tracking-tight lg:text-2xl mb-4',
          className
        )}
        {...props}
      />
    ),
    h2: ({ className, ...props }) => (
      <h2
        className={cn('scroll-m-20 text-lg font-semibold tracking-tight mb-3', className)}
        {...props}
      />
    ),
    h3: ({ className, ...props }) => (
      <h3
        className={cn('scroll-m-20 text-base font-semibold tracking-tight mb-2', className)}
        {...props}
      />
    ),

    // Paragraphs
    p: ({ className, ...props }) => (
      <p className={cn('leading-7 [&:not(:first-child)]:mt-4', className)} {...props} />
    ),

    // Lists
    ul: ({ className, ...props }) => (
      <ul className={cn('my-6 ml-6 list-disc', className)} {...props} />
    ),
    ol: ({ className, ...props }) => (
      <ol className={cn('my-6 ml-6 list-decimal', className)} {...props} />
    ),
    li: ({ className, ...props }) => <li className={cn('mt-2', className)} {...props} />,

    // Blockquotes
    blockquote: ({ className, ...props }) => (
      <blockquote
        className={cn('mt-6 border-l-2 border-border pl-6 italic text-muted-foreground', className)}
        {...props}
      />
    ),

    // Links
    a: ({ className, ...props }) => (
      <a
        className={cn(
          'font-medium text-primary underline underline-offset-4 hover:text-primary/80',
          className
        )}
        {...props}
      />
    ),

    // Tables
    table: ({ className, ...props }) => (
      <div className="my-6 w-full overflow-y-auto">
        <table className={cn('w-full', className)} {...props} />
      </div>
    ),
    thead: ({ className, ...props }) => (
      <thead className={cn('[&_tr]:border-b', className)} {...props} />
    ),
    tbody: ({ className, ...props }) => (
      <tbody className={cn('[&_tr:last-child]:border-0', className)} {...props} />
    ),
    tr: ({ className, ...props }) => (
      <tr
        className={cn(
          'border-b transition-colors hover:bg-muted/50 data-[state=selected]:bg-muted',
          className
        )}
        {...props}
      />
    ),
    th: ({ className, ...props }) => (
      <th
        className={cn(
          'h-12 px-4 text-left align-middle font-medium text-muted-foreground [&:has([role=checkbox])]:pr-0',
          className
        )}
        {...props}
      />
    ),
    td: ({ className, ...props }) => (
      <td className={cn('p-4 align-middle [&:has([role=checkbox])]:pr-0', className)} {...props} />
    ),

    // Horizontal rule
    hr: ({ className, ...props }) => (
      <hr className={cn('my-4 border-border', className)} {...props} />
    ),
  }

  return (
    <div className={cn('prose prose-neutral dark:prose-invert max-w-none', className)}>
      <ReactMarkdown remarkPlugins={[remarkGfm]} components={components}>
        {children}
      </ReactMarkdown>
    </div>
  )
}
