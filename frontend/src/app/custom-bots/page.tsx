'use client';
import React from 'react';
import { withAuth } from '@/components/auth/with-auth';
import DashboardLayout from '@/components/layouts/dashboard-layout';
import CustomBotsDashboard from '@/components/custom-bots/custom-bots-dashboard';

const CustomBotsPage: React.FC = () => {
  return (
    <DashboardLayout>
      <CustomBotsDashboard />
    </DashboardLayout>
  );
};

export default withAuth(CustomBotsPage);
