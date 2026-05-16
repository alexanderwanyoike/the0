"use client";

import { useEffect } from "react";
import { useRouter } from "next/navigation";
import { useAuth } from "@/contexts/auth-context";

export default function AppEntryPage() {
  const router = useRouter();
  const { user, loading } = useAuth();

  useEffect(() => {
    if (loading) return;

    if (user) {
      router.replace("/dashboard");
      return;
    }

    fetch("/api/auth/setup-status")
      .then((response) => response.json())
      .then((body) => {
        if (body.data?.setupRequired) {
          router.replace("/setup");
        } else {
          router.replace("/login");
        }
      })
      .catch(() => router.replace("/login"));
  }, [loading, router, user]);

  return null;
}
