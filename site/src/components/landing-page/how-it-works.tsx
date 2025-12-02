import { Code, TestTube, Container, BarChart3 } from "lucide-react";

const steps = [
  {
    icon: Code,
    title: "Develop",
    description:
      "Create custom trading bots using Python or JavaScript with any libraries you prefer. Use our framework-agnostic approach to build everything from simple DCA strategies to complex multi-asset arbitrage algorithms.",
  },
  {
    icon: TestTube,
    title: "Test",
    description:
      "Validate your strategies with paper trading and unit tests. Test different parameters, optimize performance, and ensure your algorithms work before deploying to live markets.",
  },
  {
    icon: Container,
    title: "Deploy",
    description:
      "Launch your platform locally with Docker Compose in under 5 minutes. Deploy your bots for scheduled execution or real-time trading with full isolation and security.",
  },
  {
    icon: BarChart3,
    title: "Monitor",
    description:
      "Track performance through the analytics dashboard. Monitor bot execution, analyze trading results, and get comprehensive insights into your automated trading portfolio.",
  },
];

export function HowItWorksSection() {
  return (
    <section id="how-it-works" className="py-16 md:py-24 bg-muted/10">
      <div className="container max-w-5xl">
        <div className="text-center mb-16">
          <h2 className="text-3xl font-bold">How It Works</h2>
          <p className="mt-4 text-xl text-muted-foreground">
            From development to deployment in 4 simple steps
          </p>
        </div>

        <div className="grid md:grid-cols-2 gap-12">
          {steps.map((step, index) => (
            <div key={index} className="space-y-4">
              <div className="flex items-center gap-4">
                <div className="flex h-10 w-10 shrink-0 items-center justify-center rounded-lg bg-primary/10">
                  <step.icon className="h-5 w-5 text-primary" />
                </div>
                <h3 className="text-xl font-semibold">{step.title}</h3>
              </div>
              <p className="text-muted-foreground leading-relaxed">
                {step.description}
              </p>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}
