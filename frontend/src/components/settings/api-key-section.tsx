'use client';
import React, { useState, useEffect } from 'react';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { Key, Plus, Activity, Calendar, Trash2, Copy, Eye, EyeOff } from 'lucide-react';
import { useAuth } from '@/contexts/auth-context';
import { useToast } from '@/hooks/use-toast';
import { ApiKeyService, ApiKey, ApiKeyStats } from '@/lib/api/api-key-service';
import { CreateApiKeyDialog } from './create-api-key-dialog';
import { DeleteApiKeyDialog } from './delete-api-key-dialog';
import { ApiKeySetupDialog } from './api-key-setup-dialog';
import { formatDistanceToNow } from 'date-fns';

export function ApiKeySection() {
  console.log('🟢 ApiKeySection component rendered');
  const { user } = useAuth();
  const { toast } = useToast();
  const [apiKeys, setApiKeys] = useState<ApiKey[]>([]);
  const [stats, setStats] = useState<ApiKeyStats | null>(null);
  const [loading, setLoading] = useState(true);
  const [createDialogOpen, setCreateDialogOpen] = useState(false);
  const [deleteDialogOpen, setDeleteDialogOpen] = useState(false);
  const [detailsDialogOpen, setDetailsDialogOpen] = useState(false);
  const [selectedApiKey, setSelectedApiKey] = useState<ApiKey | null>(null);
  const [newApiKey, setNewApiKey] = useState<ApiKey | null>(null);
  const [visibleKeys, setVisibleKeys] = useState<Set<string>>(new Set());

  const fetchApiKeys = React.useCallback(async () => {
    if (!user) return;

    try {
      const [keysResult, statsResult] = await Promise.all([
        ApiKeyService.getApiKeys(),
        ApiKeyService.getApiKeyStats(),
      ]);

      if (keysResult.success) {
        setApiKeys(keysResult.data);
      } else {
        toast({
          title: 'Error',
          description: 'Failed to load API keys.',
          variant: 'destructive',
        });
      }

      if (statsResult.success) {
        setStats(statsResult.data);
      }
    } catch (error) {
      console.error('Error fetching API keys:', error);
      toast({
        title: 'Error',
        description: 'Failed to load API keys.',
        variant: 'destructive',
      });
    } finally {
      setLoading(false);
    }
  }, [user, toast]);

  useEffect(() => {
    fetchApiKeys();
  }, [fetchApiKeys]);

  const handleCreateApiKey = async (name: string) => {
    if (!user) return;

    try {
      const result = await ApiKeyService.createApiKey({ name });

      if (result.success) {
        // Set the new API key to show in dialog, but DON'T close the dialog yet
        setNewApiKey(result.data);
        // Refresh the list in background
        await fetchApiKeys();
        toast({
          title: 'Success',
          description:
            'Your API key has been created. Make sure to copy it now!',
        });
      } else {
        toast({
          title: 'Error',
          description: result.error.message,
          variant: 'destructive',
        });
      }
    } catch (error) {
      console.error('Error creating API key:', error);
      toast({
        title: 'Error',
        description: 'Failed to create API key.',
        variant: 'destructive',
      });
    }
  };

  const handleDeleteApiKey = async (apiKey: ApiKey) => {
    console.log('🗑️ handleDeleteApiKey called for:', apiKey.name, apiKey.id);
    if (!user) return;

    try {
      const result = await ApiKeyService.deleteApiKey(apiKey.id);

      if (result.success) {
        setDeleteDialogOpen(false);
        setSelectedApiKey(null);
        await fetchApiKeys();
        toast({
          title: 'API key revoked',
          description: 'The API key has been revoked successfully.',
        });
      } else {
        toast({
          title: 'Error',
          description: result.error.message,
          variant: 'destructive',
        });
      }
    } catch (error) {
      console.error('Error deleting API key:', error);
      toast({
        title: 'Error',
        description: 'Failed to revoke API key.',
        variant: 'destructive',
      });
    }
  };

  const copyToClipboard = (text: string, description: string = 'text') => {
    navigator.clipboard.writeText(text);
    toast({
      title: 'Copied',
      description: `${description} copied to clipboard.`,
    });
  };

  const openDeleteDialog = (apiKey: ApiKey) => {
    console.log('🗑️ openDeleteDialog called for:', apiKey.name, apiKey.id);
    setSelectedApiKey(apiKey);
    setDeleteDialogOpen(true);
  };

  const openSetupDialog = (apiKey: ApiKey) => {
    setSelectedApiKey(apiKey);
    setDetailsDialogOpen(true);
  };

  const handleCreateDialogClose = (open: boolean) => {
    setCreateDialogOpen(open);
    // Only clear newApiKey if dialog is being closed
    if (!open && newApiKey) {
      setNewApiKey(null);
    }
  };

  const handleNewApiKeyShown = () => {
    setNewApiKey(null);
  };

  const toggleKeyVisibility = (keyId: string) => {
    setVisibleKeys(prev => {
      const newSet = new Set(prev);
      if (newSet.has(keyId)) {
        newSet.delete(keyId);
      } else {
        newSet.add(keyId);
      }
      return newSet;
    });
  };

  if (loading) {
    return (
      <div className="space-y-6">
        <div className="flex items-start sm:items-center gap-3">
          <Key className="h-5 w-5 text-muted-foreground mt-0.5 sm:mt-0 flex-shrink-0" />
          <div className="min-w-0 flex-1">
            <h2 className="text-lg font-medium">API Keys</h2>
            <p className="text-sm text-muted-foreground mt-1">
              Manage API keys for programmatic access
            </p>
          </div>
        </div>
        <div className="space-y-4">
          <Skeleton className="h-20 w-full" />
          <Skeleton className="h-32 w-full" />
        </div>
      </div>
    );
  }

  return (
    <div className="space-y-6">
      <div className="flex items-start sm:items-center gap-3">
        <Key className="h-5 w-5 text-muted-foreground mt-0.5 sm:mt-0 flex-shrink-0" />
        <div className="min-w-0 flex-1">
          <h2 className="text-lg font-medium">API Keys</h2>
          <p className="text-sm text-muted-foreground mt-1">
            Manage API keys for programmatic access to the0 platform
          </p>
        </div>
      </div>

      {/* Stats Overview */}
      {stats && (
        <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
          <Card>
            <CardContent className="p-4">
              <div className="flex items-center gap-2">
                <Key className="h-4 w-4 text-muted-foreground" />
                <div className="text-sm font-medium">Total Keys</div>
              </div>
              <div className="text-2xl font-bold mt-1">{stats.total}</div>
            </CardContent>
          </Card>
          <Card>
            <CardContent className="p-4">
              <div className="flex items-center gap-2">
                <Activity className="h-4 w-4 text-green-500" />
                <div className="text-sm font-medium">Active Keys</div>
              </div>
              <div className="text-2xl font-bold mt-1">{stats.active}</div>
            </CardContent>
          </Card>
        </div>
      )}

      {/* API Keys List */}
      <Card>
        <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-4">
          <div>
            <CardTitle className="text-base">Your API Keys</CardTitle>
            <CardDescription>
              API keys allow you to authenticate with the0&#39;s API
              programmatically
            </CardDescription>
          </div>
          <Button
            onClick={() => setCreateDialogOpen(true)}
            size="sm"
            className="flex items-center gap-2"
          >
            <Plus className="h-4 w-4" />
            <span className="hidden sm:inline">Create Key</span>
          </Button>
        </CardHeader>
        <CardContent>
          {apiKeys.length === 0 ? (
            <div className="text-center py-8">
              <Key className="h-12 w-12 text-muted-foreground mx-auto mb-4" />
              <h3 className="text-lg font-medium mb-2">No API keys yet</h3>
              <p className="text-sm text-muted-foreground">
                Create your first API key to start using the0&#39;s API
                programmatically
              </p>
            </div>
          ) : (
            <div className="space-y-4">
              {apiKeys.map((apiKey) => (
                <div
                  key={apiKey.id}
                  className="flex flex-col sm:flex-row sm:items-center justify-between p-4 border rounded-lg space-y-3 sm:space-y-0"
                >
                  <div className="flex-1 min-w-0">
                    <div className="flex items-center gap-2 mb-2">
                      <h4 className="font-medium truncate">{apiKey.name}</h4>
                      <Badge
                        variant={apiKey.isActive ? 'default' : 'secondary'}
                      >
                        {apiKey.isActive ? 'Active' : 'Inactive'}
                      </Badge>
                    </div>
                    <div className="space-y-1">
                      <div className="flex items-center gap-2 text-sm text-muted-foreground">
                        <span className="font-mono text-xs">
                          {visibleKeys.has(apiKey.id) 
                            ? apiKey.key
                            : '••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••'
                          }
                        </span>
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => toggleKeyVisibility(apiKey.id)}
                          className="h-6 w-6 p-0"
                          title={visibleKeys.has(apiKey.id) ? 'Hide API key' : 'Show API key'}
                        >
                          {visibleKeys.has(apiKey.id) ? (
                            <EyeOff className="h-3 w-3" />
                          ) : (
                            <Eye className="h-3 w-3" />
                          )}
                        </Button>
                        {visibleKeys.has(apiKey.id) && (
                          <Button
                            variant="ghost"
                            size="sm"
                            onClick={() =>
                              copyToClipboard(
                                apiKey.key,
                                'API key',
                              )
                            }
                            className="h-6 w-6 p-0"
                            title="Copy API key"
                          >
                            <Copy className="h-3 w-3" />
                          </Button>
                        )}
                      </div>
                      <div className="flex flex-col sm:flex-row sm:items-center gap-1 sm:gap-4 text-xs text-muted-foreground">
                        <div className="flex items-center gap-1">
                          <Calendar className="h-3 w-3" />
                          Created{' '}
                          {formatDistanceToNow(new Date(apiKey.createdAt), {
                            addSuffix: true,
                          })}
                        </div>
                        {apiKey.lastUsedAt && (
                          <div className="flex items-center gap-1">
                            <Activity className="h-3 w-3" />
                            Last used{' '}
                            {formatDistanceToNow(new Date(apiKey.lastUsedAt), {
                              addSuffix: true,
                            })}
                          </div>
                        )}
                      </div>
                    </div>
                  </div>
                  <div className="flex items-center gap-2">
                    <Button
                      variant="outline"
                      size="sm"
                      onClick={() => openSetupDialog(apiKey)}
                    >
                      <Key className="h-4 w-4 sm:mr-2" />
                      <span className="hidden sm:inline">Setup</span>
                    </Button>
                    <Button
                      variant="outline"
                      size="sm"
                      onClick={() => {
                        console.log('🔴 Revoke button clicked!', apiKey.name);
                        openDeleteDialog(apiKey);
                      }}
                      className="text-destructive hover:text-destructive"
                    >
                      <Trash2 className="h-4 w-4 sm:mr-2" />
                      <span className="hidden sm:inline">Revoke</span>
                    </Button>
                  </div>
                </div>
              ))}
            </div>
          )}
        </CardContent>
      </Card>

      {/* Dialogs */}
      <CreateApiKeyDialog
        open={createDialogOpen}
        onOpenChange={handleCreateDialogClose}
        onCreateApiKey={handleCreateApiKey}
        newApiKey={newApiKey}
        onNewApiKeyShown={handleNewApiKeyShown}
      />

      {selectedApiKey && (
        <>
          <DeleteApiKeyDialog
            open={deleteDialogOpen}
            onOpenChange={setDeleteDialogOpen}
            apiKey={selectedApiKey}
            onDeleteApiKey={handleDeleteApiKey}
          />
          <ApiKeySetupDialog
            open={detailsDialogOpen}
            onOpenChange={setDetailsDialogOpen}
            apiKey={selectedApiKey}
          />
        </>
      )}
    </div>
  );
}
