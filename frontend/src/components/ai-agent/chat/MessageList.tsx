import { useEffect, useRef, useState, useCallback } from "react";
import { ScrollArea } from "@/components/ui/scroll-area";
import { MessageItem } from "./MessageItem";
import { Message } from "@/types";

interface MessageListProps {
  messages: Message[];
}

export function MessageList({ messages }: MessageListProps) {
  const scrollAreaRef = useRef<HTMLDivElement>(null);
  const [isUserScrolling, setIsUserScrolling] = useState(false);
  const [shouldAutoScroll, setShouldAutoScroll] = useState(true);
  const scrollTimeoutRef = useRef<NodeJS.Timeout | undefined>(undefined);

  // Function to scroll to bottom smoothly
  const scrollToBottom = useCallback((smooth = true) => {
    if (scrollAreaRef.current) {
      const viewport = scrollAreaRef.current.querySelector(
        "[data-radix-scroll-area-viewport]",
      );
      if (viewport) {
        viewport.scrollTo({
          top: viewport.scrollHeight,
          behavior: smooth ? "smooth" : "instant",
        });
      }
    }
  }, []);

  // Check if user is near bottom (within 100px)
  const isNearBottom = useCallback(() => {
    if (scrollAreaRef.current) {
      const viewport = scrollAreaRef.current.querySelector(
        "[data-radix-scroll-area-viewport]",
      );
      if (viewport) {
        const threshold = 100;
        const distanceFromBottom =
          viewport.scrollHeight - viewport.scrollTop - viewport.clientHeight;
        return distanceFromBottom <= threshold;
      }
    }
    return true;
  }, []);

  // Handle scroll events to detect user scrolling
  const handleScroll = useCallback(() => {
    if (!isNearBottom()) {
      setIsUserScrolling(true);
      setShouldAutoScroll(false);
    } else {
      setIsUserScrolling(false);
      setShouldAutoScroll(true);
    }

    // Clear existing timeout
    if (scrollTimeoutRef.current) {
      clearTimeout(scrollTimeoutRef.current);
    }

    // Set timeout to re-enable auto-scroll after user stops scrolling
    scrollTimeoutRef.current = setTimeout(() => {
      if (isNearBottom()) {
        setIsUserScrolling(false);
        setShouldAutoScroll(true);
      }
    }, 1000);
  }, [isNearBottom]);

  // Auto-scroll when messages change or content updates
  useEffect(() => {
    if (shouldAutoScroll && !isUserScrolling) {
      // Use a small delay to allow DOM to update
      const timeoutId = setTimeout(() => {
        scrollToBottom(true);
      }, 50);

      return () => clearTimeout(timeoutId);
    }
  }, [messages, shouldAutoScroll, isUserScrolling, scrollToBottom]);

  // Set up scroll listener
  useEffect(() => {
    if (scrollAreaRef.current) {
      const viewport = scrollAreaRef.current.querySelector(
        "[data-radix-scroll-area-viewport]",
      );
      if (viewport) {
        viewport.addEventListener("scroll", handleScroll);
        return () => {
          viewport.removeEventListener("scroll", handleScroll);
          if (scrollTimeoutRef.current) {
            clearTimeout(scrollTimeoutRef.current);
          }
        };
      }
    }
  }, [handleScroll]);

  // Auto-scroll to bottom on initial load
  useEffect(() => {
    scrollToBottom(false);
  }, [scrollToBottom]);

  return (
    <ScrollArea ref={scrollAreaRef} className="h-full">
      <div className="p-4 space-y-4">
        {messages.map((message) => (
          <MessageItem
            key={message.id}
            message={message}
            onContentUpdate={() => {
              // Trigger scroll when streaming content updates
              if (shouldAutoScroll && !isUserScrolling) {
                setTimeout(() => scrollToBottom(true), 16); // Next frame
              }
            }}
          />
        ))}
      </div>

      {/* Show scroll-to-bottom button when user has scrolled up */}
      {isUserScrolling && (
        <button
          onClick={() => {
            setIsUserScrolling(false);
            setShouldAutoScroll(true);
            scrollToBottom(true);
          }}
          className="fixed bottom-20 right-8 bg-primary text-primary-foreground rounded-full p-2 shadow-lg hover:bg-primary/90 transition-colors z-10"
          aria-label="Scroll to bottom"
        >
          <svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor">
            <path d="M12 16l-6-6h12l-6 6z" />
          </svg>
        </button>
      )}
    </ScrollArea>
  );
}
