import { Navigation } from "@/components/landing-page/navigation";
import { Footer } from "@/components/landing-page/footer";
import { HeroSection } from "@/components/landing-page/hero";
import { FeaturesSection } from "@/components/landing-page/features";

export default function LandingPage() {
  return (
    <div className="min-h-screen">
      <Navigation />
      <main>
        <HeroSection />
        <FeaturesSection />
      </main>
      <Footer />
    </div>
  );
}
