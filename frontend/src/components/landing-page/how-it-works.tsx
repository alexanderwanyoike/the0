import { MousePointer, Settings, Rocket, LayoutDashboard } from 'lucide-react';

const steps = [
  {
    icon: MousePointer,
    title: 'Choose',
    description:
      'Browse our library of proven trading strategies. From conservative dollar-cost averaging to dynamic momentum trading—find the approach that matches your investment goals and risk tolerance.',
  },
  {
    icon: Settings,
    title: 'Configure',
    description:
      'Set your parameters through our simple interface. Choose your assets, set your budget, pick your schedule, and customize risk settings. No technical knowledge required.',
  },
  {
    icon: Rocket,
    title: 'Deploy',
    description:
      'Connect your brokerage account and launch your bot with one click. Your automated trading strategy starts working immediately, executing trades based on your configured rules.',
  },
  // {
  //   icon: LayoutDashboard,
  //   title: 'Monitor',
  //   description:
  //     'Track performance through your personalized dashboard. View profits, adjust settings, pause or modify bots anytime. Stay in complete control of your automated investments.',
  // },
];

export function HowItWorksSection() {
  return (
    <section id="how-it-works" className="py-16 md:py-24 bg-muted/10">
      <div className="container max-w-5xl">
        <div className="text-center mb-16">
          <h2 className="text-3xl font-bold">How It Works</h2>
          <p className="mt-4 text-xl text-muted-foreground">
            From setup to profits in just 3 simple steps
          </p>
        </div>

        <div className="grid md:grid-cols-2 gap-12">
          {steps.map((step, index) => (
            <div
              key={index}
              className={`space-y-4 ${
                index === steps.length - 1 && steps.length % 2 !== 0
                  ? 'md:col-span-2 md:mx-auto md:max-w-md'
                  : ''
              }`}
            >
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
