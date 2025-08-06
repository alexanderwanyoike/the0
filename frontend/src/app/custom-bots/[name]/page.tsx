"use client";
import React from "react";
import { useParams } from "next/navigation";
import { withAuth } from "@/components/auth/with-auth";
import DashboardLayout from "@/components/layouts/dashboard-layout";
import { CustomBotDetailPage } from "@/components/custom-bots/custom-bot-detail-page";

interface CustomBotDetailPageProps {
  params: { name: string };
}

const CustomBotDetailPageWrapper: React.FC = () => {
  const params = useParams();
  const botName = params?.name as string;

  if (!botName) {
    return (
      <DashboardLayout>
        <div className="container max-w-7xl mx-auto py-6 px-4 lg:px-6">
          <div className="text-center py-16">
            <h1 className="text-2xl font-bold text-destructive">
              Invalid Bot Name
            </h1>
            <p className="text-muted-foreground mt-2">
              The bot name parameter is missing.
            </p>
          </div>
        </div>
      </DashboardLayout>
    );
  }

  return (
    <DashboardLayout>
      <CustomBotDetailPage botName={decodeURIComponent(botName)} />
    </DashboardLayout>
  );
};

export default withAuth(CustomBotDetailPageWrapper);
