"use client";

import { use, useEffect } from "react";
import { useRouter } from "next/navigation";

interface BotRedirectProps {
  params: Promise<{ botId: string }>;
}

export default function BotRedirect({ params }: BotRedirectProps) {
  const { botId } = use(params);
  const router = useRouter();

  useEffect(() => {
    router.replace(`/dashboard/${botId}`);
  }, [botId, router]);

  return null;
}
