"use client";
import React from "react";
import { AuthGate } from "./auth-gate";

export function withAuth<P extends object>(Component: React.ComponentType<P>) {
  const ProtectedRoute = function (props: P) {
    return (
      <AuthGate>
        <Component {...props} />
      </AuthGate>
    );
  };

  ProtectedRoute.displayName = "ProtectedRoute";
  return ProtectedRoute;
}
