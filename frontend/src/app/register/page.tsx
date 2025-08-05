"use client";

import { useEffect } from "react";
import { useRouter } from "next/navigation";
import { useAuth } from "@/contexts/auth-context";
import { RegisterForm } from "@/components/auth/register-form";
import { NavigationMenu } from "@/components/landing-page/navigation-menu";

export default function RegisterPage() {
  const router = useRouter();
  const { user } = useAuth();

  useEffect(() => {
    if (user) {
      router.push("/dashboard");
    }
  }, [user, router]);

  return (
    <div className="min-h-screen bg-background">
      {/* Navigation */}
      <NavigationMenu />

      {/* Login Form */}
      <div className="container flex items-center justify-center min-h-[calc(100vh-4rem)] p-4">
        <div className="w-full max-w-md">
          <RegisterForm />
        </div>
      </div>
    </div>
  );
}
