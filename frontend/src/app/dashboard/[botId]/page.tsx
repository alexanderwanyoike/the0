"use client";

import { use } from "react";
import { BotDetailPanel } from "@/components/dashboard/bot-detail-panel";

interface BotDetailPageProps {
  params: Promise<{ botId: string }>;
}

export default function BotDetailPage({ params }: BotDetailPageProps) {
  const { botId } = use(params);
  return <BotDetailPanel botId={botId} />;
}
