import { Bot, ChevronDown, ChevronRight, Terminal } from 'lucide-react';
import React from 'react';

interface DocsNavigationProps {
  navigation: {
    bots: { id: string; title: string }[];
    cli: { id: string; title: string }[];
  };
  expandedSections: {
    bots: boolean;
    cli: boolean;
  };
  setExpandedSections: React.Dispatch<
    React.SetStateAction<{
      bots: boolean;
      cli: boolean;
    }>
  >;
}

const DocsNavigation = ({
  navigation,
  expandedSections,
  setExpandedSections,
}: DocsNavigationProps) => {
  return (
    <div className="p-4">
      {/* Bots Section */}
      <div className="pb-4">
        <button
          onClick={() =>
            setExpandedSections((prev) => ({ ...prev, bots: !prev.bots }))
          }
          className="flex items-center gap-2 text-sm font-medium w-full px-2 py-1.5 rounded-md hover:bg-accent/50 transition-colors"
        >
          {expandedSections.bots ? (
            <ChevronDown className="h-4 w-4 text-muted-foreground" />
          ) : (
            <ChevronRight className="h-4 w-4 text-muted-foreground" />
          )}
          <Bot className="h-4 w-4 text-primary" />
          <span>Trading Bots</span>
        </button>
        {expandedSections.bots && (
          <div className="mt-1 ml-6 space-y-1">
            {navigation.bots.map((item) => (
              <button
                key={item.id}
                className="w-full text-left px-2 py-1.5 text-sm rounded-md hover:bg-accent/50 transition-colors text-muted-foreground hover:text-foreground"
              >
                {item.title}
              </button>
            ))}
          </div>
        )}
      </div>

      {/* CLI Section */}
      <div>
        <button
          onClick={() =>
            setExpandedSections((prev) => ({ ...prev, cli: !prev.cli }))
          }
          className="flex items-center gap-2 text-sm font-medium w-full px-2 py-1.5 rounded-md hover:bg-accent/50 transition-colors"
        >
          {expandedSections.cli ? (
            <ChevronDown className="h-4 w-4 text-muted-foreground" />
          ) : (
            <ChevronRight className="h-4 w-4 text-muted-foreground" />
          )}
          <Terminal className="h-4 w-4 text-primary" />
          <span>CLI Usage</span>
        </button>
        {expandedSections.cli && (
          <div className="mt-1 ml-6 space-y-1">
            {navigation.cli.map((item) => (
              <button
                key={item.id}
                className="w-full text-left px-2 py-1.5 text-sm rounded-md hover:bg-accent/50 transition-colors text-muted-foreground hover:text-foreground"
              >
                {item.title}
              </button>
            ))}
          </div>
        )}
      </div>
    </div>
  );
};

export default DocsNavigation;
