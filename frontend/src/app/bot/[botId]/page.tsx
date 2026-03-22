import { redirect } from "next/navigation";

interface BotRedirectProps {
  params: Promise<{ botId: string }>;
}

export default async function BotRedirect({ params }: BotRedirectProps) {
  const { botId } = await params;
  redirect(`/dashboard/${botId}`);
}
