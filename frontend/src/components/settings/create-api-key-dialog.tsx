'use client';
import React, { useState, useEffect } from 'react';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Alert, AlertDescription, AlertTitle } from '@/components/ui/alert';
import {
  Copy,
  Eye,
  EyeOff,
  Key,
  AlertTriangle,
  CheckCircle,
} from 'lucide-react';
import { ApiKey } from '@/lib/api/api-key-service';
import { useToast } from '@/hooks/use-toast';

interface CreateApiKeyDialogProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  onCreateApiKey: (name: string) => Promise<void>;
  newApiKey: ApiKey | null;
  onNewApiKeyShown: () => void;
}

export function CreateApiKeyDialog({
  open,
  onOpenChange,
  onCreateApiKey,
  newApiKey,
  onNewApiKeyShown,
}: CreateApiKeyDialogProps) {
  const { toast } = useToast();
  const [name, setName] = useState('');
  const [loading, setLoading] = useState(false);
  const [showKey, setShowKey] = useState(false);
  const [hasBeenCopied, setHasBeenCopied] = useState(false);

  // Reset state when dialog opens/closes
  useEffect(() => {
    if (open && !newApiKey) {
      // Opening for creation
      setName('');
      setShowKey(false);
      setHasBeenCopied(false);
    }
  }, [open, newApiKey]);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!name.trim()) return;

    setLoading(true);
    try {
      await onCreateApiKey(name.trim());
      // Don't reset name here - let the success state handle it
    } finally {
      setLoading(false);
    }
  };

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text);
    setHasBeenCopied(true);
    toast({
      title: 'API Key Copied!',
      description:
        'Your API key has been copied to clipboard. Store it securely.',
    });
  };

  const handleDone = () => {
    // Clear the new API key and reset state
    onNewApiKeyShown();
    setName('');
    setShowKey(false);
    setHasBeenCopied(false);
    onOpenChange(false);
  };

  const handleCancel = () => {
    if (newApiKey) {
      // If we have a new key, warn the user
      if (!hasBeenCopied) {
        const confirmed = window.confirm(
          "Are you sure you want to close without copying your API key? You won't be able to see it again.",
        );
        if (!confirmed) return;
      }
      handleDone();
    } else {
      // Just close normally
      setName('');
      onOpenChange(false);
    }
  };

  // If we have a new API key, show the success screen
  if (newApiKey) {
    return (
      <Dialog open={open} onOpenChange={handleCancel}>
        <DialogContent
          className="sm:max-w-lg"
          onPointerDownOutside={(e) => e.preventDefault()}
        >
          <DialogHeader>
            <DialogTitle className="flex items-center gap-2">
              <Key className="h-5 w-5 text-green-600" />
              API Key Created Successfully
            </DialogTitle>
            <DialogDescription>
              Your new API key has been created. <strong>Copy it now</strong> -
              you won&#39;t be able to see it again for security reasons.
            </DialogDescription>
          </DialogHeader>

          <Alert variant={'destructive'}>
            <AlertTriangle />
            <AlertTitle> Important Security Notice</AlertTitle>
            <AlertDescription>
              This is the only time you&#39;ll see the full API key. Make sure
              to copy and store it securely before closing this dialog.
            </AlertDescription>
          </Alert>

          <div className="space-y-4">
            <div>
              <Label className="text-sm font-medium">API Key Name</Label>
              <Input value={newApiKey.name} disabled className="mt-1" />
            </div>

            <div>
              <div className="flex items-center justify-between mb-2">
                <Label className="text-sm font-medium">Your API Key</Label>
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={() => setShowKey(!showKey)}
                  className="h-6 px-2"
                >
                  {showKey ? (
                    <>
                      <EyeOff className="h-3 w-3 mr-1" />
                      Hide
                    </>
                  ) : (
                    <>
                      <Eye className="h-3 w-3 mr-1" />
                      Show
                    </>
                  )}
                </Button>
              </div>
              <div className="flex gap-2">
                <Input
                  value={
                    showKey
                      ? newApiKey.key
                      : '••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••'
                  }
                  disabled
                  className="font-mono text-sm"
                />
                <Button
                  variant={hasBeenCopied ? 'default' : 'outline'}
                  size="sm"
                  onClick={() => copyToClipboard(newApiKey.key)}
                  className={
                    hasBeenCopied ? 'bg-green-600 hover:bg-green-700' : ''
                  }
                >
                  {hasBeenCopied ? (
                    <>
                      <CheckCircle className="h-4 w-4 mr-1" />
                      Copied
                    </>
                  ) : (
                    <>
                      <Copy className="h-4 w-4 mr-1" />
                      Copy
                    </>
                  )}
                </Button>
              </div>
              {hasBeenCopied && (
                <p className="text-sm text-green-600 mt-2 flex items-center gap-1">
                  <CheckCircle className="h-3 w-3" />
                  API key copied to clipboard!
                </p>
              )}
            </div>
          </div>

          <div className="flex justify-between items-center pt-4">
            <div className="text-sm text-muted-foreground">
              {hasBeenCopied
                ? 'You can now safely close this dialog.'
                : 'Remember to copy your key before closing.'}
            </div>
            <Button
              onClick={handleDone}
              variant={hasBeenCopied ? 'default' : 'outline'}
            >
              {hasBeenCopied ? 'Done' : 'Close Without Copying'}
            </Button>
          </div>
        </DialogContent>
      </Dialog>
    );
  }

  // Creation form
  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent className="sm:max-w-md">
        <DialogHeader>
          <DialogTitle className="flex items-center gap-2">
            <Key className="h-5 w-5" />
            Create New API Key
          </DialogTitle>
          <DialogDescription>
            Create a new API key for programmatic access to THE0 platform.
          </DialogDescription>
        </DialogHeader>

        <form onSubmit={handleSubmit} className="space-y-4">
          <div>
            <Label htmlFor="name" className="text-sm font-medium">
              Key Name <span className="text-red-500">*</span>
            </Label>
            <Input
              id="name"
              value={name}
              onChange={(e) => setName(e.target.value)}
              placeholder="e.g., My Trading Bot Key, Production API Access"
              className="mt-1"
              required
              minLength={3}
              maxLength={50}
              disabled={loading}
            />
            <p className="text-xs text-muted-foreground mt-1">
              Choose a descriptive name to help you identify this key later
              (3-50 characters).
            </p>
          </div>

          <div className="flex justify-end gap-3 pt-4">
            <Button
              type="button"
              variant="outline"
              onClick={() => onOpenChange(false)}
              disabled={loading}
            >
              Cancel
            </Button>
            <Button
              type="submit"
              disabled={!name.trim() || loading}
              className="min-w-[120px]"
            >
              {loading ? (
                <>
                  <div className="animate-spin rounded-full h-4 w-4 border-b-2 border-white mr-2"></div>
                  Creating...
                </>
              ) : (
                'Create API Key'
              )}
            </Button>
          </div>
        </form>
      </DialogContent>
    </Dialog>
  );
}
