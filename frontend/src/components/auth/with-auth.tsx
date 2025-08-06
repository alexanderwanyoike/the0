"use client";
import React, { useEffect } from "react";
import { useRouter } from "next/navigation";
import { useAuth } from "@/contexts/auth-context";

export function withAuth<P extends object>(Component: React.ComponentType<P>) {
  const ProtectedRoute = function (props: P) {
    const { user, loading } = useAuth();
    const router = useRouter();

    useEffect(() => {
      if (!loading && !user) {
        router.replace("/login");
      }
    }, [user, loading, router]);

    // Show nothing while loading or redirecting
    if (loading || !user) {
      return null;
    }

    // If authenticated, render component
    return <Component {...props} />;
  };

  ProtectedRoute.displayName = "ProtectedRoute";
  return ProtectedRoute;
}
