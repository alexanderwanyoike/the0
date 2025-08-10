import React from "react";
import { Github, Code, Users, Heart } from "lucide-react";
import { Button } from "@/components/ui/button";
import { Navigation } from "@/components/landing-page/navigation";
import { Footer } from "@/components/landing-page/footer";
import { DOCS_URLS } from "@/lib/constants";

export default function AboutPage() {
  return (
    <div className="min-h-screen bg-background">
      <Navigation />
      {/* Hero Section */}
      <div className="py-16 md:py-24">
        <div className="container max-w-4xl">
          <div className="text-center">
            <div className="mb-6 flex justify-center">
              <div className="bg-primary w-12 h-12 rounded flex items-center justify-center">
                <span className="text-primary-foreground font-mono font-bold text-2xl">
                  0
                </span>
              </div>
            </div>
            <h1 className="text-4xl font-bold mb-4">About the0</h1>
            <p className="text-xl text-muted-foreground max-w-2xl mx-auto">
              An open source algorithmic trading platform that empowers
              developers and traders to create, deploy, and manage trading bots
              across multiple markets. Built by the community, for the
              community.
            </p>
          </div>
        </div>
      </div>

      {/* Mission Section */}
      <div className="py-16 bg-muted/50">
        <div className="container max-w-4xl">
          <div className="text-center mb-12">
            <h2 className="text-2xl font-bold mb-4">Our Mission</h2>
            <p className="text-lg text-muted-foreground">
              We believe algorithmic trading should be accessible to everyone.
              the0 provides the infrastructure and tools needed to build
              everything from simple DCA strategies to complex multi-asset
              arbitrage algorithms.
            </p>
          </div>

          <div className="grid md:grid-cols-3 gap-8 text-center">
            <div>
              <div className="bg-primary/10 rounded-lg p-6 mb-4">
                <Code className="h-8 w-8 mx-auto mb-4 text-primary" />
                <h3 className="font-bold mb-2">Developer First</h3>
                <p className="text-muted-foreground">
                  Framework agnostic approach. Build bots in Python or
                  JavaScript with any libraries you prefer. Full flexibility and
                  control.
                </p>
              </div>
            </div>
            <div>
              <div className="bg-primary/10 rounded-lg p-6 mb-4">
                <Users className="h-8 w-8 mx-auto mb-4 text-primary" />
                <h3 className="font-bold mb-2">Open Source</h3>
                <p className="text-muted-foreground">
                  Built in the open with community contributions. Transparent,
                  secure, and continuously improved by developers worldwide.
                </p>
              </div>
            </div>
            <div>
              <div className="bg-primary/10 rounded-lg p-6 mb-4">
                <Heart className="h-8 w-8 mx-auto mb-4 text-primary" />
                <h3 className="font-bold mb-2">Community Driven</h3>
                <p className="text-muted-foreground">
                  We encourage the use of AI tools, creative solutions, and
                  innovative approaches to algorithmic trading development.
                </p>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Technology Section */}
      <div className="py-16">
        <div className="container max-w-4xl">
          <div className="text-center mb-12">
            <h2 className="text-2xl font-bold mb-4">Technology Stack</h2>
            <p className="text-muted-foreground">
              Built with modern technologies for scalability, performance, and
              reliability.
            </p>
          </div>

          <div className="grid md:grid-cols-2 gap-8">
            <div className="space-y-6">
              <div>
                <h3 className="font-bold mb-2 text-primary">
                  Microservices Architecture
                </h3>
                <p className="text-muted-foreground">
                  Comprehensive platform with specialized services for bot
                  execution, backtesting, scheduling, and security analysis.
                  Each service is designed for scalability and reliability.
                </p>
              </div>

              <div>
                <h3 className="font-bold mb-2 text-primary">Modern Frontend</h3>
                <p className="text-muted-foreground">
                  Next.js 15 with React 19 for the web dashboard, plus a
                  Go-based CLI tool for local development. Real-time monitoring
                  and comprehensive bot management.
                </p>
              </div>
            </div>

            <div className="space-y-6">
              <div>
                <h3 className="font-bold mb-2 text-primary">Container Ready</h3>
                <p className="text-muted-foreground">
                  Docker Compose for local development, Kubernetes support for
                  production deployments. Get up and running in minutes with a
                  single command.
                </p>
              </div>

              <div>
                <h3 className="font-bold mb-2 text-primary">Multi-Database</h3>
                <p className="text-muted-foreground">
                  PostgreSQL for user data, MongoDB for runtime state, MinIO for
                  storage, and NATS JetStream for event coordination. Each
                  database optimized for its use case.
                </p>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Contributing Section */}
      <div className="py-16 bg-muted/50">
        <div className="container max-w-4xl">
          <div className="text-center mb-12">
            <h2 className="text-2xl font-bold mb-4">Join the Community</h2>
            <p className="text-muted-foreground">
              We welcome contributions from developers, traders, and AI
              enthusiasts. Help us build the future of algorithmic trading.
            </p>
          </div>

          <div className="grid md:grid-cols-2 gap-8">
            <div className="space-y-6">
              <div>
                <h3 className="font-bold mb-2 text-primary">
                  How to Contribute
                </h3>
                <ul className="text-muted-foreground space-y-2">
                  <li>• Submit bug reports and feature requests</li>
                  <li>• Contribute code improvements and new features</li>
                  <li>• Share innovative trading strategies and patterns</li>
                  <li>• Help improve documentation and examples</li>
                  <li>• Test and provide feedback on new releases</li>
                </ul>
              </div>
            </div>

            <div className="space-y-6">
              <div>
                <h3 className="font-bold mb-2 text-primary">
                  AI-Friendly Development
                </h3>
                <p className="text-muted-foreground mb-4">
                  We encourage the use of AI tools like Claude, ChatGPT, and
                  GitHub Copilot in development. Creative solutions and
                  innovative approaches are always welcome.
                </p>
                <Button asChild>
                  <a
                    href={DOCS_URLS.github}
                    target="_blank"
                    rel="noopener noreferrer"
                    className="gap-2"
                  >
                    <Github className="h-4 w-4" />
                    View on GitHub
                  </a>
                </Button>
              </div>
            </div>
          </div>
        </div>
      </div>

      <Footer />
    </div>
  );
}
