"use client";
import { ProfileSection } from "@/components/settings/profile-section";
import { withAuth } from "@/components/auth/with-auth";

function ProfileSettingsPage() {
  return <ProfileSection />;
}

export default withAuth(ProfileSettingsPage);
