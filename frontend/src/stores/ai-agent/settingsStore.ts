import { create } from "zustand";
import { persist } from "zustand/middleware";

interface SettingsStore {
  streamingEnabled: boolean;
  typewriterEnabled: boolean;
  typewriterSpeed: number; // Characters per second
  setStreamingEnabled: (enabled: boolean) => void;
  setTypewriterEnabled: (enabled: boolean) => void;
  setTypewriterSpeed: (speed: number) => void;
}

export const useSettingsStore = create<SettingsStore>()(
  persist(
    (set) => ({
      streamingEnabled: true, // Default to enabled
      typewriterEnabled: true, // Default to enabled
      typewriterSpeed: 50, // Default 50 characters per second
      setStreamingEnabled: (enabled) => set({ streamingEnabled: enabled }),
      setTypewriterEnabled: (enabled) => set({ typewriterEnabled: enabled }),
      setTypewriterSpeed: (speed) =>
        set({ typewriterSpeed: Math.max(10, Math.min(200, speed)) }), // Clamp between 10-200
    }),
    {
      name: "settings-storage",
    },
  ),
);
