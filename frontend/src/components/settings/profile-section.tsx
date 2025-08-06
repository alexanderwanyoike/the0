import { ProfileForm } from "./profile-form";
import { PasswordForm } from "./password-form";
import { DeleteAccount } from "./delete-account";
import { Separator } from "@/components/ui/separator";

export function ProfileSection() {
  return (
    <div className="space-y-8">
      <ProfileForm />
      <Separator />
      <PasswordForm />
      <Separator />
      <DeleteAccount />
    </div>
  );
}
