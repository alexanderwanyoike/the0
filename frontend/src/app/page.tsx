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

    const controller = new AbortController();
    let active = true;

    fetch("/api/auth/setup-status", { signal: controller.signal })
      .then((response) => response.json())
      .then((body) => {
        if (!active) return;
        if (body.data?.setupRequired) {
          router.replace("/setup");
        } else {
          router.replace("/login");
        }
      })
      .catch((error) => {
        if (!active || error?.name === "AbortError") return;
        router.replace("/login");
      });

    return () => {
      active = false;
      controller.abort();
    };
  }, [loading, router, user]);

  return null;
}
