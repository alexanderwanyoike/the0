"use client";

import { useEffect } from "react";
import { useRouter } from "next/navigation";
import { AuthBackground } from "@/components/auth/auth-background";
import { LoginForm } from "@/components/auth/login-form";
import { useAuth } from "@/contexts/auth-context";
import { Skeleton } from "@/components/ui/skeleton";

function LoginPageSkeleton() {
  return (
    <AuthBackground>
      <div className="w-full max-w-md space-y-6">
        <div className="space-y-2 text-center">
          <Skeleton className="h-8 w-32 mx-auto" />
          <Skeleton className="h-4 w-48 mx-auto" />
        </div>
        <div className="space-y-4">
          <div className="space-y-2">
            <Skeleton className="h-4 w-12" />
            <Skeleton className="h-10 w-full" />
          </div>
          <div className="space-y-2">
            <Skeleton className="h-4 w-16" />
            <Skeleton className="h-10 w-full" />
          </div>
          <Skeleton className="h-10 w-full" />
        </div>
      </div>
    </AuthBackground>
  );
}

export default function LoginPage() {
  const router = useRouter();
  const { user, loading } = useAuth();

  useEffect(() => {
    if (!loading && user) {
      router.push("/dashboard");
    }
  }, [user, loading, router]);

  if (loading || user) {
    return <LoginPageSkeleton />;
  }

  return (
    <AuthBackground>
      <LoginForm />
    </AuthBackground>
  );
}
