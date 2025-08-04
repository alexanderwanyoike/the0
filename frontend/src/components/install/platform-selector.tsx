'use client';

import { useState } from 'react';
import { getAllPlatforms } from '@/lib/install/platform-detection';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select';
import { Badge } from '@/components/ui/badge';
import { Laptop, Monitor, Smartphone } from 'lucide-react';
import type { PlatformInfo, PlatformSelectorProps } from '@/types/install';

export function PlatformSelector({
  platforms,
  selectedPlatform,
  onPlatformChange,
}: PlatformSelectorProps) {
  const [selectedId, setSelectedId] = useState<string>(
    selectedPlatform?.id || '',
  );

  const handleSelectionChange = (platformId: string) => {
    setSelectedId(platformId);
    const platform = platforms.find((p) => p.id === platformId);
    if (platform) {
      onPlatformChange(platform);
    }
  };

  return (
    <div className="space-y-4">
      <div>
        <h3 className="text-lg font-semibold mb-2">Select Your Platform</h3>
        <p className="text-sm text-muted-foreground">
          Choose your operating system and architecture to get the correct
          installation command.
        </p>
      </div>

      {/* Dropdown selector */}
      <Select value={selectedId} onValueChange={handleSelectionChange}>
        <SelectTrigger className="w-full">
          <SelectValue placeholder="Select your platform..." />
        </SelectTrigger>
        <SelectContent>
          {platforms.map((platform) => (
            <SelectItem key={platform.id} value={platform.id}>
              <div className="flex items-center space-x-2">
                <PlatformIcon platform={platform} />
                <span>{platform.displayName}</span>
              </div>
            </SelectItem>
          ))}
        </SelectContent>
      </Select>

      {/* Platform cards for visual selection */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
        {platforms.map((platform) => (
          <PlatformCard
            key={platform.id}
            platform={platform}
            isSelected={selectedId === platform.id}
            onSelect={() => handleSelectionChange(platform.id)}
          />
        ))}
      </div>
    </div>
  );
}

// Individual platform card component
function PlatformCard({
  platform,
  isSelected,
  onSelect,
}: {
  platform: PlatformInfo;
  isSelected: boolean;
  onSelect: () => void;
}) {
  return (
    <Card
      className={`cursor-pointer transition-all hover:shadow-md ${
        isSelected ? 'ring-2 ring-primary border-primary' : ''
      }`}
      onClick={onSelect}
    >
      <CardHeader className="pb-3">
        <div className="flex items-center justify-between">
          <div className="flex items-center space-x-2">
            <PlatformIcon platform={platform} />
            <CardTitle className="text-base">{platform.os}</CardTitle>
          </div>
          {isSelected && (
            <Badge variant="default" className="text-xs">
              Selected
            </Badge>
          )}
        </div>
      </CardHeader>
      <CardContent className="pt-0">
        <div className="space-y-1">
          <p className="text-sm font-medium">{platform.displayName}</p>
          <p className="text-xs text-muted-foreground">
            Architecture: {platform.arch}
          </p>
          <p className="text-xs text-muted-foreground">
            Shell: {platform.shellCommand}
          </p>
        </div>
      </CardContent>
    </Card>
  );
}

// Platform icon component
function PlatformIcon({ platform }: { platform: PlatformInfo }) {
  const iconClass = 'h-4 w-4';

  if (platform.os === 'macOS') {
    return <Laptop className={iconClass} />;
  } else if (platform.os === 'Windows') {
    return <Monitor className={iconClass} />;
  } else if (platform.os === 'Linux') {
    return <Smartphone className={iconClass} />;
  }

  return <Monitor className={iconClass} />;
}

// Compact platform selector for smaller spaces
export function CompactPlatformSelector({
  platforms,
  selectedPlatform,
  onPlatformChange,
}: PlatformSelectorProps) {
  const handleSelectionChange = (platformId: string) => {
    const platform = platforms.find((p) => p.id === platformId);
    if (platform) {
      onPlatformChange(platform);
    }
  };

  return (
    <div className="flex items-center space-x-2">
      <span className="text-sm font-medium whitespace-nowrap">Platform:</span>
      <Select
        value={selectedPlatform?.id || ''}
        onValueChange={handleSelectionChange}
      >
        <SelectTrigger className="w-48">
          <SelectValue placeholder="Select platform" />
        </SelectTrigger>
        <SelectContent>
          {platforms.map((platform) => (
            <SelectItem key={platform.id} value={platform.id}>
              {platform.displayName}
            </SelectItem>
          ))}
        </SelectContent>
      </Select>
    </div>
  );
}

// Platform selector with radio buttons (alternative UI)
export function RadioPlatformSelector({
  platforms,
  selectedPlatform,
  onPlatformChange,
}: PlatformSelectorProps) {
  const [selectedId, setSelectedId] = useState<string>(
    selectedPlatform?.id || '',
  );

  const handleSelectionChange = (platformId: string) => {
    setSelectedId(platformId);
    const platform = platforms.find((p) => p.id === platformId);
    if (platform) {
      onPlatformChange(platform);
    }
  };

  return (
    <div className="space-y-3">
      <h3 className="text-lg font-semibold">Choose Your Platform</h3>
      <div className="space-y-2">
        {platforms.map((platform) => (
          <label
            key={platform.id}
            className="flex items-center space-x-3 p-3 rounded-md border cursor-pointer hover:bg-muted/50 transition-colors"
          >
            <input
              type="radio"
              name="platform"
              value={platform.id}
              checked={selectedId === platform.id}
              onChange={() => handleSelectionChange(platform.id)}
              className="h-4 w-4 text-primary focus:ring-primary"
            />
            <div className="flex items-center space-x-2">
              <PlatformIcon platform={platform} />
              <div>
                <p className="font-medium">{platform.displayName}</p>
                <p className="text-sm text-muted-foreground">
                  {platform.arch} • {platform.shellCommand}
                </p>
              </div>
            </div>
          </label>
        ))}
      </div>
    </div>
  );
}

// Auto-populated platform selector that uses detection as default
export function AutoPlatformSelector({
  detectedPlatform,
  onPlatformChange,
}: {
  detectedPlatform: PlatformInfo | null;
  onPlatformChange: (platform: PlatformInfo) => void;
}) {
  const allPlatforms = getAllPlatforms();
  const [selectedPlatform, setSelectedPlatform] = useState<PlatformInfo | null>(
    detectedPlatform,
  );

  const handlePlatformChange = (platform: PlatformInfo) => {
    setSelectedPlatform(platform);
    onPlatformChange(platform);
  };

  return (
    <div className="space-y-4">
      {detectedPlatform && (
        <div className="p-3 bg-green-50 dark:bg-green-950/30 border border-green-200 dark:border-green-800 rounded-md">
          <p className="text-sm font-medium text-green-800 dark:text-green-200">
            Detected Platform: {detectedPlatform.displayName}
          </p>
          <p className="text-xs text-green-600 dark:text-green-400 mt-1">
            The installation command below is configured for your platform.
          </p>
        </div>
      )}

      <div className="flex items-center justify-between">
        <h3 className="text-lg font-semibold">Platform Selection</h3>
        {detectedPlatform && (
          <Button
            variant="outline"
            size="sm"
            onClick={() => handlePlatformChange(detectedPlatform)}
          >
            Use Detected
          </Button>
        )}
      </div>

      <CompactPlatformSelector
        platforms={allPlatforms}
        selectedPlatform={selectedPlatform}
        onPlatformChange={handlePlatformChange}
      />
    </div>
  );
}
