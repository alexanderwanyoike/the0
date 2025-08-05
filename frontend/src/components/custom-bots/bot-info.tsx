import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Bot } from "lucide-react";
import { Badge } from "@/components/ui/badge";
import { Separator } from "@/components/ui/separator";
import React from "react";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";

export const BotInfo = ({ bot, selectedVersion, onVersionChange }: any) => {
  const versionData =
    bot.versions.find((v: any) => v.version === selectedVersion) ||
    bot.versions[0];

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <Bot className="h-5 w-5" />
          Bot Information
        </CardTitle>
      </CardHeader>
      <CardContent className="space-y-4">
        {/* Version Selector */}
        <div className="space-y-2">
          <label className="text-sm font-medium">Version</label>
          <Select value={selectedVersion} onValueChange={onVersionChange}>
            <SelectTrigger>
              <SelectValue placeholder="Select version" />
            </SelectTrigger>
            <SelectContent>
              {bot.versions.map((version: any) => (
                <SelectItem key={version.version} value={version.version}>
                  <div className="flex items-center gap-2">
                    <span>v{version.version}</span>
                    <Badge
                      variant={
                        version.status === "approved" ? "default" : "secondary"
                      }
                      className="text-xs"
                    >
                      {version.status}
                    </Badge>
                  </div>
                </SelectItem>
              ))}
            </SelectContent>
          </Select>
        </div>

        <Separator />

        <div className="space-y-3">
          <div className="flex justify-between">
            <span className="text-muted-foreground">Name:</span>
            <span className="font-medium">
              {versionData.config?.name || bot.config.name}
            </span>
          </div>
          <div className="flex justify-between">
            <span className="text-muted-foreground">Type:</span>
            <Badge variant="secondary">
              {versionData.config?.type || bot.config.type}
            </Badge>
          </div>
          <div className="flex justify-between">
            <span className="text-muted-foreground">Author:</span>
            <span className="font-medium">
              {versionData.config?.author || bot.config.author}
            </span>
          </div>
          <div className="flex justify-between">
            <span className="text-muted-foreground">Created:</span>
            <span className="font-medium">
              {versionData.createdAt.toLocaleDateString()}
            </span>
          </div>
          <div className="flex justify-between">
            <span className="text-muted-foreground">Status:</span>
            <Badge
              variant={
                versionData.status === "approved" ? "default" : "secondary"
              }
            >
              {versionData.status}
            </Badge>
          </div>
        </div>

        <Separator />

        <div className="space-y-2">
          <h4 className="font-medium">Description</h4>
          <p className="text-sm text-muted-foreground">
            {versionData.config?.description || bot.config.description}
          </p>
        </div>
      </CardContent>
    </Card>
  );
};
