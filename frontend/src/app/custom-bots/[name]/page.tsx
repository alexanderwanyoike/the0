"use client";

import { use } from "react";
import { CustomBotDetailPanel } from "@/components/custom-bots/custom-bot-detail-panel";

interface CustomBotDetailPageProps {
  params: Promise<{ name: string }>;
}

export default function CustomBotDetailPage({
  params,
}: CustomBotDetailPageProps) {
  const { name } = use(params);
  return <CustomBotDetailPanel botName={decodeURIComponent(name)} />;
}
