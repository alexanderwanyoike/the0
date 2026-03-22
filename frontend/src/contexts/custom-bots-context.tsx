"use client";

import React, { createContext, useContext } from "react";
import {
  useCustomBots,
  UseCustomBotsReturn,
} from "@/hooks/custom-bots/use-custom-bots";

const CustomBotsContext = createContext<UseCustomBotsReturn | null>(null);

export function CustomBotsProvider({
  children,
}: {
  children: React.ReactNode;
}) {
  const value = useCustomBots();

  return (
    <CustomBotsContext.Provider value={value}>
      {children}
    </CustomBotsContext.Provider>
  );
}

export function useCustomBotsContext() {
  const ctx = useContext(CustomBotsContext);
  if (!ctx) {
    throw new Error(
      "useCustomBotsContext must be used within CustomBotsProvider",
    );
  }
  return ctx;
}
