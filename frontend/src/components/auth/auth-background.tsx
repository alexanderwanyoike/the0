import { NodeNetwork } from "./node-network";

export function AuthBackground({ children }: { children: React.ReactNode }) {
  return (
    <div className="relative min-h-screen overflow-hidden bg-zinc-950">
      <NodeNetwork />
      <div className="absolute inset-0 bg-gradient-to-b from-zinc-950/30 via-zinc-950/55 to-zinc-950/85" />
      <div className="relative z-10 flex min-h-screen items-center justify-center p-4">
        {children}
      </div>
    </div>
  );
}
