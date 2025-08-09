import { Navigation } from "@/components/landing-page/navigation";
import { Footer } from "@/components/landing-page/footer";
import { HeroSection } from "@/components/landing-page/hero";
import { FeaturesSection } from "@/components/landing-page/features";
import { QuickStartSection } from "@/components/landing-page/quick-start";
import { CodeShowcaseSection } from "@/components/landing-page/code-showcase";
import { ArchitectureSection } from "@/components/landing-page/architecture";

export default function LandingPage() {
  return (
    <div className="min-h-screen">
      <Navigation />
      <main>
        <HeroSection />
        <QuickStartSection />
        <CodeShowcaseSection />
        <ArchitectureSection />
        <FeaturesSection />
      </main>
      <Footer />
    </div>
  );
}
