import { NavigationMenu } from "@/components/landing-page/navigation-menu";
import { HeroSection } from "@/components/landing-page/hero";
import { FeaturesSection } from "@/components/landing-page/features";
import { HowItWorksSection } from "@/components/landing-page/how-it-works";
import { Footer } from "@/components/landing-page/footer";

export default function LandingPage() {
  return (
    <div className="min-h-screen">
      <NavigationMenu showSearch={true} />
      <main>
        <HeroSection />
        <FeaturesSection />
        <HowItWorksSection />
      </main>
      <Footer />
    </div>
  );
}
