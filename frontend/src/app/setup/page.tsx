"use client";

import { useEffect, useState } from "react";
import { useRouter } from "next/navigation";
import { AuthBackground } from "@/components/auth/auth-background";
import { SetupForm } from "@/components/auth/setup-form";
import { Skeleton } from "@/components/ui/skeleton";

interface SetupStatus {
  setupRequired: boolean;
}

export default function SetupPage() {
  const router = useRouter();
  const [status, setStatus] = useState<SetupStatus | null>(null);

  useEffect(() => {
    let active = true;
    fetch("/api/auth/setup-status")
      .then((response) => response.json())
      .then((body) => {
        if (!active) return;
        const nextStatus = body.data as SetupStatus;
        setStatus(nextStatus);
        if (!nextStatus.setupRequired) {
          router.replace("/login");
        }
      })
      .catch(() => router.replace("/login"));

    return () => {
      active = false;
    };
  }, [router]);

  if (!status || !status.setupRequired) {
    return (
      <AuthBackground>
        <div className="w-full max-w-md space-y-4">
          <Skeleton className="h-8 w-40 mx-auto" />
          <Skeleton className="h-64 w-full" />
        </div>
      </AuthBackground>
    );
  }

  return (
    <AuthBackground>
      <SetupForm />
    </AuthBackground>
  );
}
