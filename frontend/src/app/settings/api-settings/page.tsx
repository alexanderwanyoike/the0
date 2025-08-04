'use client';
import { ProfileSection } from '@/components/settings/profile-section';
import { withAuth } from '@/components/auth/with-auth';
import { ApiSection } from '@/components/settings/api-section';

function ApiSettings() {
  return <ApiSection />;
}

export default withAuth(ApiSettings);
