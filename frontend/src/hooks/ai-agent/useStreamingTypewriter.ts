import { useState, useEffect, useRef, useCallback } from "react";

interface UseStreamingTypewriterOptions {
  speed?: number; // Characters per second
  enabled?: boolean;
  onUpdate?: () => void; // Called when display text changes
}

export function useStreamingTypewriter(
  options: UseStreamingTypewriterOptions = {},
) {
  const { speed = 50, enabled = true, onUpdate } = options;
  const [displayText, setDisplayText] = useState("");
  const [isTyping, setIsTyping] = useState(false);

  const queueRef = useRef<string[]>([]);
  const currentTextRef = useRef("");
  const intervalRef = useRef<number | null>(null);
  const isProcessingRef = useRef(false);

  // Clear any existing interval
  const clearInterval = useCallback(() => {
    if (intervalRef.current) {
      window.clearInterval(intervalRef.current);
      intervalRef.current = null;
    }
  }, []);

  // Process the queue of text chunks
  const processQueue = useCallback(() => {
    if (isProcessingRef.current || queueRef.current.length === 0) {
      return;
    }

    if (!enabled) {
      // If typewriter disabled, show all queued content immediately
      const allText = queueRef.current.join("");
      queueRef.current = [];
      currentTextRef.current += allText;
      setDisplayText(currentTextRef.current);
      onUpdate?.();
      return;
    }

    isProcessingRef.current = true;
    setIsTyping(true);

    const intervalMs = 1000 / speed;
    let currentChunk = queueRef.current.shift() || "";
    let chunkIndex = 0;

    intervalRef.current = window.setInterval(() => {
      if (chunkIndex >= currentChunk.length) {
        // Current chunk complete, get next chunk
        if (queueRef.current.length > 0) {
          currentChunk = queueRef.current.shift() || "";
          chunkIndex = 0;
          return;
        }

        // No more chunks, stop typing
        clearInterval();
        setIsTyping(false);
        isProcessingRef.current = false;
        return;
      }

      // Add 1-3 characters at a time for natural feel
      const charsToAdd = Math.min(
        Math.floor(Math.random() * 3) + 1,
        currentChunk.length - chunkIndex,
      );

      const newChars = currentChunk.slice(chunkIndex, chunkIndex + charsToAdd);
      chunkIndex += charsToAdd;

      currentTextRef.current += newChars;
      setDisplayText(currentTextRef.current);
      onUpdate?.();
    }, intervalMs);
  }, [enabled, speed, clearInterval, onUpdate]);

  // Add new content chunk to the queue
  const addChunk = useCallback(
    (chunk: string) => {
      if (!chunk) return;

      queueRef.current.push(chunk);
      processQueue();
    },
    [processQueue],
  );

  // Set complete content immediately (bypass typewriter)
  const setCompleteText = useCallback(
    (text: string) => {
      clearInterval();
      queueRef.current = [];
      currentTextRef.current = text;
      setDisplayText(text);
      onUpdate?.();
      setIsTyping(false);
      isProcessingRef.current = false;
    },
    [clearInterval, onUpdate],
  );

  // Reset everything
  const reset = useCallback(() => {
    clearInterval();
    queueRef.current = [];
    currentTextRef.current = "";
    setDisplayText("");
    setIsTyping(false);
    isProcessingRef.current = false;
  }, [clearInterval]);

  // Force complete current typing
  const complete = useCallback(() => {
    if (queueRef.current.length === 0 && !isProcessingRef.current) return;

    clearInterval();
    const remainingText = queueRef.current.join("");
    queueRef.current = [];
    currentTextRef.current += remainingText;
    setDisplayText(currentTextRef.current);
    onUpdate?.();
    setIsTyping(false);
    isProcessingRef.current = false;
  }, [clearInterval, onUpdate]);

  // Cleanup on unmount
  useEffect(() => {
    return () => {
      clearInterval();
    };
  }, [clearInterval]);

  return {
    displayText,
    isTyping,
    addChunk,
    setCompleteText,
    reset,
    complete,
  };
}
