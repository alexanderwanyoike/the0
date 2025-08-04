'use client';

import React, { useState, useRef, useEffect } from 'react';
import { Search, X } from 'lucide-react';
import { Input } from '@/components/ui/input';
import { Button } from '@/components/ui/button';
import { useRouter } from 'next/navigation';
import { cn } from '@/lib/utils';
import { DocSearchResult } from '@/lib/docs/search-service';

interface DocsSearchProps {
  placeholder?: string;
  className?: string;
}

export const DocsSearch: React.FC<DocsSearchProps> = ({
  placeholder = 'Search documentation...',
  className,
}) => {
  const [query, setQuery] = useState('');
  const [isOpen, setIsOpen] = useState(false);
  const [results, setResults] = useState<DocSearchResult[]>([]);
  const [suggestions, setSuggestions] = useState<string[]>([]);
  const [isLoading, setIsLoading] = useState(false);
  const router = useRouter();
  const inputRef = useRef<HTMLInputElement>(null);
  const containerRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (
        containerRef.current &&
        !containerRef.current.contains(event.target as Node)
      ) {
        setIsOpen(false);
      }
    };

    document.addEventListener('mousedown', handleClickOutside);
    return () => document.removeEventListener('mousedown', handleClickOutside);
  }, []);

  const handleInputChange = async (e: React.ChangeEvent<HTMLInputElement>) => {
    const value = e.target.value;
    setQuery(value);

    if (value.length >= 2) {
      setIsLoading(true);
      setIsOpen(true);

      try {
        // Fetch both search results and suggestions
        const [searchResponse, suggestionsResponse] = await Promise.all([
          fetch(`/api/docs/search?q=${encodeURIComponent(value)}&type=search`),
          fetch(
            `/api/docs/search?q=${encodeURIComponent(value)}&type=suggestions`,
          ),
        ]);

        const searchData = await searchResponse.json();
        const suggestionsData = await suggestionsResponse.json();

        if (searchData.success) {
          setResults(searchData.data);
        }
        if (suggestionsData.success) {
          setSuggestions(suggestionsData.data);
        }
      } catch (error) {
        console.error('Search error:', error);
      } finally {
        setIsLoading(false);
      }
    } else {
      setIsOpen(false);
      setResults([]);
      setSuggestions([]);
    }
  };

  const handleResultClick = (result: DocSearchResult) => {
    setQuery('');
    setIsOpen(false);
    router.push(`/docs/${result.path}`);
  };

  const handleSuggestionClick = (suggestion: string) => {
    setQuery(suggestion);
    inputRef.current?.focus();
    // Trigger search with suggestion
    handleInputChange({
      target: { value: suggestion },
    } as React.ChangeEvent<HTMLInputElement>);
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Escape') {
      setIsOpen(false);
    }
  };

  const handleClear = () => {
    setQuery('');
    setIsOpen(false);
    setResults([]);
    setSuggestions([]);
    inputRef.current?.focus();
  };

  return (
    <div ref={containerRef} className={cn('relative w-full', className)}>
      <div className="relative">
        <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-muted-foreground" />
        <Input
          ref={inputRef}
          type="text"
          placeholder={placeholder}
          value={query}
          onChange={handleInputChange}
          onKeyDown={handleKeyDown}
          onFocus={() => query.length >= 2 && setIsOpen(true)}
          className="pl-10 pr-10"
        />
        {query && (
          <Button
            variant="ghost"
            size="sm"
            onClick={handleClear}
            className="absolute right-2 top-1/2 transform -translate-y-1/2 h-6 w-6 p-0 hover:bg-muted"
          >
            <X className="h-3 w-3" />
          </Button>
        )}
      </div>

      {/* Results Dropdown */}
      {isOpen && query.length >= 2 && (
        <div className="absolute top-full left-0 right-0 z-50 mt-1 bg-background border border-border rounded-md shadow-lg max-h-96 overflow-y-auto">
          {isLoading ? (
            <div className="p-4 text-center text-muted-foreground">
              <div className="animate-spin h-4 w-4 border-2 border-primary border-t-transparent rounded-full mx-auto"></div>
              <p className="mt-2 text-sm">Searching...</p>
            </div>
          ) : (
            <>
              {/* Search Results */}
              {results.length > 0 && (
                <div className="border-b border-border">
                  <div className="px-3 py-2 text-xs font-medium text-muted-foreground bg-muted/50">
                    Results
                  </div>
                  {results.map((result, index) => (
                    <button
                      key={index}
                      onClick={() => handleResultClick(result)}
                      className="w-full text-left px-4 py-3 hover:bg-muted transition-colors border-b border-border/50 last:border-b-0"
                    >
                      <div className="flex items-start gap-3">
                        <Search className="h-4 w-4 text-muted-foreground mt-0.5 flex-shrink-0" />
                        <div className="flex-1 min-w-0">
                          <div className="font-medium text-sm line-clamp-1">
                            {result.title}
                          </div>
                          <div className="text-xs text-muted-foreground mt-1 line-clamp-2">
                            {result.excerpt}
                          </div>
                          <div className="text-xs text-muted-foreground/60 mt-1">
                            {result.path}
                          </div>
                        </div>
                      </div>
                    </button>
                  ))}
                </div>
              )}

              {/* Suggestions */}
              {suggestions.length > 0 && (
                <div>
                  <div className="px-3 py-2 text-xs font-medium text-muted-foreground bg-muted/50">
                    Suggestions
                  </div>
                  {suggestions.map((suggestion, index) => (
                    <button
                      key={index}
                      onClick={() => handleSuggestionClick(suggestion)}
                      className="w-full text-left px-4 py-2 hover:bg-muted text-sm transition-colors border-b border-border/50 last:border-b-0"
                    >
                      <div className="flex items-center gap-2">
                        <Search className="h-3 w-3 text-muted-foreground" />
                        {suggestion}
                      </div>
                    </button>
                  ))}
                </div>
              )}

              {/* No Results */}
              {!isLoading &&
                results.length === 0 &&
                suggestions.length === 0 && (
                  <div className="p-4 text-center text-muted-foreground">
                    <p className="text-sm">
                      No results found for &quot;{query}&quot;
                    </p>
                  </div>
                )}
            </>
          )}
        </div>
      )}
    </div>
  );
};
