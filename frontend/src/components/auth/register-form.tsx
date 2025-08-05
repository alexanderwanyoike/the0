"use client";
import React, { useState } from "react";
import { useRouter } from "next/navigation";
import { JwtAuthService } from "@/lib/auth/jwt-auth.service";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { Alert, AlertDescription } from "@/components/ui/alert";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { APP_NAME } from "@/lib/constants";
import { useAuth } from "@/contexts/auth-context";

export function RegisterForm() {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [confirmPassword, setConfirmPassword] = useState("");
  const [error, setError] = useState<string | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const router = useRouter();
  const { register } = useAuth();

  const handleSubmit = async (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    setIsLoading(true);
    setError(null);

    if (password !== confirmPassword) {
      setError("Passwords do not match");
      setIsLoading(false);
      return;
    }

    try {
      // Need to add username for JWT auth registration
      const username = email.split("@")[0]; // Simple username from email
      const result = await register({
        username,
        email,
        password,
      });

      if (!result.success) {
        setError(result.error || "Registration failed");
      }
      // Success navigation is handled by the auth context
    } catch (error) {
      const errorMessage = formatErrorMessages(error);
      setError(errorMessage);
    } finally {
      setIsLoading(false);
    }
  };

  const formatErrorMessages = (error: any) => {
    if (error instanceof Error) {
      // Format Firebase error messages to be more user-friendly
      let errorMessage = error.message;
      if (errorMessage.includes("email-already-in-use")) {
        errorMessage =
          "This email is already registered. Please try logging in instead.";
      } else if (errorMessage.includes("weak-password")) {
        errorMessage = "Password should be at least 6 characters long.";
      } else if (errorMessage.includes("invalid-email")) {
        errorMessage = "Please enter a valid email address.";
      }
      return errorMessage;
    } else {
      // Handle other types of errors
      return "An unknown error occurred. Please try again later.";
    }
  };

  return (
    <Card className="w-full max-w-md">
      <CardHeader className="space-y-1">
        <CardTitle className="text-2xl font-bold text-center">
          {APP_NAME} Registration
        </CardTitle>
        <CardDescription className="text-center">
          Create a new account to get started
        </CardDescription>
      </CardHeader>
      <CardContent>
        <form onSubmit={handleSubmit} className="space-y-4">
          <div className="space-y-2">
            <Label htmlFor="email">Email</Label>
            <Input
              id="email"
              type="email"
              placeholder="name@quanttrader.com"
              value={email}
              onChange={(e) => setEmail(e.target.value)}
              required
              disabled={isLoading}
            />
          </div>
          <div className="space-y-2">
            <Label htmlFor="password">Password</Label>
            <Input
              id="password"
              type="password"
              placeholder="••••••••"
              value={password}
              onChange={(e) => setPassword(e.target.value)}
              required
              disabled={isLoading}
            />
          </div>
          <div className="space-y-2">
            <Label htmlFor="confirm-password">Confirm Password</Label>
            <Input
              id="confirm-password"
              type="password"
              placeholder="••••••••"
              value={confirmPassword}
              onChange={(e) => setConfirmPassword(e.target.value)}
              required
              disabled={isLoading}
            />
          </div>

          {error && (
            <Alert variant="destructive">
              <AlertDescription>{error}</AlertDescription>
            </Alert>
          )}

          <Button type="submit" className="w-full" disabled={isLoading}>
            {isLoading ? "Creating account..." : "Create Account"}
          </Button>

          <p className="text-center text-sm text-muted-foreground">
            Already have an account?{" "}
            <Button
              variant="link"
              className="p-0"
              onClick={() => router.push("/login")}
            >
              Login here
            </Button>
          </p>
        </form>
      </CardContent>
    </Card>
  );
}
