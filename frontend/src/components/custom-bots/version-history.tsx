import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { GitBranch, Package, ChevronLeft, ChevronRight } from "lucide-react";
import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import React, { useState } from "react";

export const VersionHistory = ({
  versions,
  selectedVersion,
  onVersionChange,
}: any) => {
  const [currentPage, setCurrentPage] = useState(0);
  const versionsPerPage = 5;
  const totalPages = Math.ceil(versions.length / versionsPerPage);

  // Calculate current versions to display
  const startIndex = currentPage * versionsPerPage;
  const endIndex = startIndex + versionsPerPage;
  const currentVersions = versions.slice(startIndex, endIndex);

  const goToNextPage = () => {
    if (currentPage < totalPages - 1) {
      setCurrentPage(currentPage + 1);
    }
  };

  const goToPreviousPage = () => {
    if (currentPage > 0) {
      setCurrentPage(currentPage - 1);
    }
  };

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <GitBranch className="h-5 w-5" />
          Version History
        </CardTitle>
        <CardDescription>
          All versions of this bot
          {versions.length > versionsPerPage && (
            <span className="ml-2 text-xs">
              ({versions.length} total versions)
            </span>
          )}
        </CardDescription>
      </CardHeader>
      <CardContent>
        <div className="space-y-3">
          {currentVersions.map((version: any) => (
            <div
              key={version.version}
              className={`flex items-center justify-between p-3 border rounded-lg cursor-pointer transition-colors ${
                selectedVersion === version.version
                  ? "bg-primary/5 border-primary"
                  : "hover:bg-muted/50"
              }`}
              onClick={() => onVersionChange(version.version)}
            >
              <div className="flex items-center gap-3">
                <div className="flex items-center justify-center h-8 w-8 rounded-full bg-primary/10">
                  <Package className="h-4 w-4 text-primary" />
                </div>
                <div>
                  <div className="font-medium">v{version.version}</div>
                  <div className="text-xs text-muted-foreground">
                    {version.createdAt.toLocaleDateString()}
                  </div>
                </div>
              </div>
              <Badge variant="default">{version.status}</Badge>
            </div>
          ))}
        </div>

        {/* Pagination Controls */}
        {totalPages > 1 && (
          <div className="flex items-center justify-between mt-4 pt-4 border-t">
            <div className="text-xs text-muted-foreground">
              Page {currentPage + 1} of {totalPages}
            </div>
            <div className="flex items-center gap-2">
              <Button
                variant="outline"
                size="sm"
                onClick={goToPreviousPage}
                disabled={currentPage === 0}
              >
                <ChevronLeft className="h-4 w-4" />
              </Button>
              <Button
                variant="outline"
                size="sm"
                onClick={goToNextPage}
                disabled={currentPage === totalPages - 1}
              >
                <ChevronRight className="h-4 w-4" />
              </Button>
            </div>
          </div>
        )}
      </CardContent>
    </Card>
  );
};
