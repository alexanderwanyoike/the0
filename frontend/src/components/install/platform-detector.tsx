'use client';

import { useState, useEffect } from 'react';
import {
  detectPlatform,
  detectPlatformWithConfidence,
} from '@/lib/install/platform-detection';
import type { PlatformInfo, PlatformDetectorProps } from '@/types/install';

export function PlatformDetector({
  onPlatformDetected,
  children,
}: PlatformDetectorProps) {
  const [isDetecting, setIsDetecting] = useState(true);
  const [detectedPlatform, setDetectedPlatform] = useState<PlatformInfo | null>(
    null,
  );
  const [confidence, setConfidence] = useState<'high' | 'medium' | 'low'>(
    'medium',
  );
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const performDetection = () => {
      try {
        // Use enhanced detection with confidence scoring
        const result = detectPlatformWithConfidence();

        setDetectedPlatform(result.platform);
        setConfidence(result.confidence);
        setIsDetecting(false);

        // Notify parent component
        onPlatformDetected(result.platform);

        // Log detection details for debugging
        if (process.env.NODE_ENV === 'development') {
          console.log('Platform detection result:', {
            platform: result.platform?.displayName,
            confidence: result.confidence,
            method: result.detectionMethod,
            fallback: result.fallback,
          });
        }
      } catch (err) {
        console.error('Platform detection failed:', err);
        setError('Failed to detect platform');
        setIsDetecting(false);
        onPlatformDetected(null);
      }
    };

    // Small delay to ensure DOM is ready
    const timer = setTimeout(performDetection, 100);

    return () => clearTimeout(timer);
  }, [onPlatformDetected]);

  // Loading state
  if (isDetecting) {
    return (
      <div
        className="flex items-center space-x-2"
        role="status"
        aria-label="Detecting platform"
      >
        <div className="h-4 w-4 animate-spin rounded-full border-2 border-primary border-t-transparent" />
        <span className="text-sm text-muted-foreground">
          Detecting platform...
        </span>
      </div>
    );
  }

  // Error state
  if (error) {
    return (
      <div
        className="flex items-center space-x-2"
        data-testid="detection-error-indicator"
      >
        <div className="h-2 w-2 rounded-full bg-red-500" />
        <span className="text-sm text-red-600">{error}</span>
      </div>
    );
  }

  // Success state
  if (detectedPlatform) {
    const confidenceColor = {
      high: 'bg-green-500',
      medium: 'bg-yellow-500',
      low: 'bg-orange-500',
    }[confidence];

    const confidenceText = {
      high: 'Confident detection',
      medium: 'Likely match',
      low: 'Best guess',
    }[confidence];

    return (
      <div
        className="flex items-center space-x-2"
        data-testid="detection-success-indicator"
      >
        <div className={`h-2 w-2 rounded-full ${confidenceColor}`} />
        <span className="text-sm font-medium">
          Detected: {detectedPlatform.displayName}
        </span>
        {confidence !== 'high' && (
          <span
            className="text-xs text-muted-foreground"
            title={confidenceText}
          >
            ({confidence} confidence)
          </span>
        )}
      </div>
    );
  }

  // No platform detected
  return (
    <div
      className="flex items-center space-x-2"
      data-testid="detection-failure-indicator"
    >
      <div className="h-2 w-2 rounded-full bg-yellow-500" />
      <span className="text-sm text-muted-foreground">
        Platform not detected - please select manually
      </span>
    </div>
  );
}

// Alternative compact version for smaller UI contexts
export function CompactPlatformDetector({
  onPlatformDetected,
}: Pick<PlatformDetectorProps, 'onPlatformDetected'>) {
  const [detectedPlatform, setDetectedPlatform] = useState<PlatformInfo | null>(
    null,
  );
  const [isDetecting, setIsDetecting] = useState(true);

  useEffect(() => {
    const platform = detectPlatform();
    setDetectedPlatform(platform);
    setIsDetecting(false);
    onPlatformDetected(platform);
  }, [onPlatformDetected]);

  if (isDetecting) {
    return (
      <div className="inline-flex items-center space-x-1">
        <div className="h-3 w-3 animate-spin rounded-full border border-primary border-t-transparent" />
        <span className="text-xs text-muted-foreground">Detecting...</span>
      </div>
    );
  }

  if (detectedPlatform) {
    return (
      <div className="inline-flex items-center space-x-1">
        <div className="h-2 w-2 rounded-full bg-green-500" />
        <span className="text-xs font-medium">
          {detectedPlatform.displayName}
        </span>
      </div>
    );
  }

  return (
    <span className="text-xs text-muted-foreground">
      Manual selection required
    </span>
  );
}

// Hook for platform detection in other components
export function usePlatformDetection() {
  const [platform, setPlatform] = useState<PlatformInfo | null>(null);
  const [isDetecting, setIsDetecting] = useState(true);
  const [confidence, setConfidence] = useState<'high' | 'medium' | 'low'>(
    'medium',
  );

  useEffect(() => {
    const result = detectPlatformWithConfidence();
    setPlatform(result.platform);
    setConfidence(result.confidence);
    setIsDetecting(false);
  }, []);

  return {
    platform,
    isDetecting,
    confidence,
    retryDetection: () => {
      setIsDetecting(true);
      const result = detectPlatformWithConfidence();
      setPlatform(result.platform);
      setConfidence(result.confidence);
      setIsDetecting(false);
    },
  };
}
