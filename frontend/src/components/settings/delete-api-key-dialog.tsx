"use client";
import React, { useState } from "react";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Button } from "@/components/ui/button";
import { Alert, AlertDescription } from "@/components/ui/alert";
import { Trash2, AlertTriangle } from "lucide-react";
import { ApiKey } from "@/lib/api/api-key-service";

interface DeleteApiKeyDialogProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  apiKey: ApiKey;
  onDeleteApiKey: (apiKey: ApiKey) => Promise<void>;
}

export function DeleteApiKeyDialog({
  open,
  onOpenChange,
  apiKey,
  onDeleteApiKey,
}: DeleteApiKeyDialogProps) {
  const [loading, setLoading] = useState(false);

  const handleDelete = async () => {
    setLoading(true);
    try {
      await onDeleteApiKey(apiKey);
    } finally {
      setLoading(false);
    }
  };

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent className="sm:max-w-2xl bg-background border shadow-lg">
        <DialogHeader>
          <DialogTitle className="flex items-center gap-2">
            <Trash2 className="h-5 w-5 text-destructive" />
            Revoke API Key
          </DialogTitle>
          <DialogDescription>
            Are you sure you want to revoke this API key? This action cannot be
            undone.
          </DialogDescription>
        </DialogHeader>

        <Alert>
          <AlertTriangle className="h-4 w-4" />
          <AlertDescription>
            <strong>Warning:</strong> Any applications using this API key will
            immediately lose access.
          </AlertDescription>
        </Alert>

        <div className="space-y-2">
          <div className="text-sm">
            <strong>Key Name:</strong> {apiKey.name}
          </div>
          <div className="text-sm">
            <strong>API Key:</strong>
            <div className="font-mono text-xs bg-muted p-2 rounded mt-1 break-all">
              {apiKey.key}
            </div>
          </div>
        </div>

        <div className="flex justify-end gap-3 pt-4">
          <Button
            variant="outline"
            onClick={() => onOpenChange(false)}
            disabled={loading}
          >
            Cancel
          </Button>
          <Button
            variant="destructive"
            onClick={handleDelete}
            disabled={loading}
          >
            {loading ? "Revoking..." : "Revoke API Key"}
          </Button>
        </div>
      </DialogContent>
    </Dialog>
  );
}
