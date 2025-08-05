import React from "react";
import Link from "next/link";
import DashboardLayout from "@/components/layouts/dashboard-layout";
import { Button } from "@/components/ui/button";
import { Bot, ArrowLeft, Home } from "lucide-react";

export default function NotFound() {
  return (
    <DashboardLayout>
      <div className="container max-w-7xl mx-auto py-6 px-4 lg:px-6">
        <div className="flex flex-col items-center justify-center py-16 px-4 text-center">
          <div className="w-20 h-20 bg-muted rounded-full flex items-center justify-center mb-6">
            <Bot className="w-10 h-10 text-muted-foreground" />
          </div>

          <h1 className="text-2xl font-bold mb-2">Custom Bot Not Found</h1>
          <p className="text-muted-foreground mb-8 max-w-md">
            The custom bot you&#39;re looking for doesn&#39;t exist or you
            don&#39;t have permission to view it.
          </p>

          <div className="flex gap-4">
            <Button asChild variant="outline">
              <Link href="/custom-bots" className="gap-2">
                <ArrowLeft className="w-4 h-4" />
                Back to Custom Bots
              </Link>
            </Button>
            <Button asChild>
              <Link href="/dashboard" className="gap-2">
                <Home className="w-4 h-4" />
                Go to Dashboard
              </Link>
            </Button>
          </div>
        </div>
      </div>
    </DashboardLayout>
  );
}
