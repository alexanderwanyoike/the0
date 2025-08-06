"use client";

import { useState, useEffect } from "react";
import { PlatformDetector } from "@/components/install/platform-detector";
import { AutoPlatformSelector } from "@/components/install/platform-selector";
import { InstallationTabs } from "@/components/install/installation-tabs";
import type { PlatformInfo } from "@/types/install";

interface InstallationInterfaceProps {
  suggestedPlatform: PlatformInfo | null;
}

export function InstallationInterface({
  suggestedPlatform,
}: InstallationInterfaceProps) {
  const [detectedPlatform, setDetectedPlatform] = useState<PlatformInfo | null>(
    null,
  );
  const [selectedPlatform, setSelectedPlatform] = useState<PlatformInfo | null>(
    suggestedPlatform,
  );
  const [showManualSelector, setShowManualSelector] = useState(false);

  // Update alternative methods and troubleshooting sections when platform changes
  useEffect(() => {
    if (selectedPlatform) {
      // Update alternative methods section
      const alternativesSection = document.getElementById(
        "alternative-methods",
      );
      if (alternativesSection && typeof window !== "undefined") {
        import("@/components/install/alternative-methods").then(
          ({ AlternativeMethods }) => {
            // This is a simplified approach - in a real implementation, you'd use React portals
            // or a more sophisticated state management solution
          },
        );
      }

      // Update troubleshooting section
      const troubleshootingSection = document.getElementById("troubleshooting");
      if (troubleshootingSection && typeof window !== "undefined") {
        import("@/components/install/troubleshooting").then(
          ({ TroubleshootingSection }) => {
            // Similar approach for troubleshooting
          },
        );
      }
    }
  }, [selectedPlatform]);

  const handlePlatformDetected = (platform: PlatformInfo | null) => {
    setDetectedPlatform(platform);
    if (platform && !selectedPlatform) {
      setSelectedPlatform(platform);
    }
  };

  const handlePlatformChange = (platform: PlatformInfo) => {
    setSelectedPlatform(platform);
  };

  const currentPlatform = selectedPlatform || detectedPlatform;

  return (
    <div className="space-y-8">
      {/* Platform Detection */}
      <div className="space-y-4">
        <div className="flex items-center justify-between">
          <div>
            <h3 className="text-lg font-semibold">Platform Detection</h3>
            <p className="text-sm text-muted-foreground">
              We&apos;ll automatically detect your platform, or you can select
              it manually.
            </p>
          </div>
          <button
            onClick={() => setShowManualSelector(!showManualSelector)}
            className="text-sm text-blue-600 hover:text-blue-800 underline"
          >
            {showManualSelector ? "Hide" : "Show"} Manual Selection
          </button>
        </div>

        <div className="space-y-4">
          <PlatformDetector onPlatformDetected={handlePlatformDetected} />

          {showManualSelector && (
            <AutoPlatformSelector
              detectedPlatform={detectedPlatform}
              onPlatformChange={handlePlatformChange}
            />
          )}
        </div>
      </div>

      {/* Installation Methods */}
      <div className="space-y-4">
        <div>
          <h3 className="text-lg font-semibold">Installation Methods</h3>
          <p className="text-sm text-muted-foreground">
            Choose how you&apos;d like to install THE0 CLI on your system.
          </p>
        </div>

        <InstallationTabs platform={currentPlatform} />
      </div>

      {/* Platform-specific information */}
      {currentPlatform && (
        <PlatformSpecificSections platform={currentPlatform} />
      )}
    </div>
  );
}

// Platform-specific sections that update based on selection
function PlatformSpecificSections({ platform }: { platform: PlatformInfo }) {
  return (
    <>
      {/* This component handles updating the alternative methods and troubleshooting sections */}
      <AlternativeMethodsUpdater platform={platform} />
      <TroubleshootingUpdater platform={platform} />
    </>
  );
}

// Component to update alternative methods section
function AlternativeMethodsUpdater({ platform }: { platform: PlatformInfo }) {
  useEffect(() => {
    const updateAlternativeMethods = async () => {
      const section = document.getElementById("alternative-methods");
      if (!section) return;

      try {
        const { AlternativeMethods } = await import(
          "@/components/install/alternative-methods"
        );
        const { createRoot } = await import("react-dom/client");

        // Clear existing content
        section.innerHTML = "";

        // Create a container for React component
        const container = document.createElement("div");
        section.appendChild(container);

        // Render the component
        const root = createRoot(container);
        root.render(<AlternativeMethods platform={platform} />);

        // Cleanup function
        return () => {
          root.unmount();
        };
      } catch (error) {
        console.error("Failed to update alternative methods:", error);
        section.innerHTML = `<p class="text-center text-muted-foreground py-8">Alternative methods for ${platform.displayName}</p>`;
      }
    };

    updateAlternativeMethods();
  }, [platform]);

  return null; // This component doesn't render anything directly
}

// Component to update troubleshooting section
function TroubleshootingUpdater({ platform }: { platform: PlatformInfo }) {
  useEffect(() => {
    const updateTroubleshooting = async () => {
      const section = document.getElementById("troubleshooting");
      if (!section) return;

      try {
        const { TroubleshootingSection } = await import(
          "@/components/install/troubleshooting"
        );
        const { createRoot } = await import("react-dom/client");

        // Clear existing content
        section.innerHTML = "";

        // Create a container for React component
        const container = document.createElement("div");
        section.appendChild(container);

        // Render the component
        const root = createRoot(container);
        root.render(<TroubleshootingSection platform={platform.id} />);

        // Cleanup function
        return () => {
          root.unmount();
        };
      } catch (error) {
        console.error("Failed to update troubleshooting:", error);
        section.innerHTML = `<p class="text-center text-muted-foreground py-8">Troubleshooting for ${platform.displayName}</p>`;
      }
    };

    updateTroubleshooting();
  }, [platform]);

  return null; // This component doesn't render anything directly
}
