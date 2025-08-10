import { useState, useEffect, useRef, useCallback } from "react";

interface UseTypewriterOptions {
  speed?: number; // Characters per second
  enabled?: boolean;
}

export function useTypewriter(options: UseTypewriterOptions = {}) {
  const { speed = 50, enabled = true } = options;
  const [displayText, setDisplayText] = useState("");
  const [isTyping, setIsTyping] = useState(false);
  const [isComplete, setIsComplete] = useState(true);

  const targetTextRef = useRef("");
  const currentPositionRef = useRef(0);
  const intervalRef = useRef<number | null>(null);
  const bufferRef = useRef<string[]>([]);
  const processingRef = useRef(false);

  // Clear any existing interval
  const clearInterval = useCallback(() => {
    if (intervalRef.current) {
      window.clearInterval(intervalRef.current);
      intervalRef.current = null;
    }
  }, []);

  // Start the typewriter animation
  const startTyping = useCallback(() => {
    if (!enabled) return;

    clearInterval();
    setIsTyping(true);
    setIsComplete(false);

    const intervalMs = 1000 / speed;

    intervalRef.current = window.setInterval(() => {
      const targetText = targetTextRef.current;
      const currentPos = currentPositionRef.current;

      if (currentPos >= targetText.length) {
        // Check if there are more chunks in buffer
        if (bufferRef.current.length > 0) {
          const nextChunk = bufferRef.current.shift()!;
          targetTextRef.current += nextChunk;
          return; // Continue typing
        }

        // Animation complete
        clearInterval();
        setIsTyping(false);
        setIsComplete(true);
        return;
      }

      // Advance by 1-3 characters for more natural feel
      const charsToAdd = Math.min(
        Math.floor(Math.random() * 3) + 1,
        targetText.length - currentPos,
      );

      currentPositionRef.current += charsToAdd;
      setDisplayText(targetText.slice(0, currentPositionRef.current));
    }, intervalMs);
  }, [enabled, speed, clearInterval]);

  // Add new content to be typed
  const addContent = useCallback(
    (newContent: string) => {
      if (!enabled) {
        // If typewriter is disabled, show content immediately
        setDisplayText((prev) => prev + newContent);
        return;
      }

      if (!processingRef.current) {
        // First chunk - start immediately
        targetTextRef.current += newContent;
        processingRef.current = true;
        startTyping();
      } else {
        // Subsequent chunks - add to buffer
        bufferRef.current.push(newContent);
      }
    },
    [enabled, startTyping],
  );

  // Set complete content immediately (bypass typewriter)
  const setContent = useCallback(
    (content: string) => {
      clearInterval();
      targetTextRef.current = content;
      currentPositionRef.current = content.length;
      setDisplayText(content);
      setIsTyping(false);
      setIsComplete(true);
      processingRef.current = false;
      bufferRef.current = [];
    },
    [clearInterval],
  );

  // Reset everything
  const reset = useCallback(() => {
    clearInterval();
    targetTextRef.current = "";
    currentPositionRef.current = 0;
    setDisplayText("");
    setIsTyping(false);
    setIsComplete(true);
    processingRef.current = false;
    bufferRef.current = [];
  }, [clearInterval]);

  // Force complete current typing
  const complete = useCallback(() => {
    if (!processingRef.current) return;

    clearInterval();
    const fullText = targetTextRef.current + bufferRef.current.join("");
    targetTextRef.current = fullText;
    currentPositionRef.current = fullText.length;
    setDisplayText(fullText);
    setIsTyping(false);
    setIsComplete(true);
    bufferRef.current = [];
  }, [clearInterval]);

  // Cleanup on unmount
  useEffect(() => {
    return () => {
      clearInterval();
    };
  }, [clearInterval]);

  return {
    displayText,
    isTyping,
    isComplete,
    addContent,
    setContent,
    reset,
    complete,
  };
}
