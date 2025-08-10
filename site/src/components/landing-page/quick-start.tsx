"use client";
import { useState } from "react";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import {
  Container,
  Download,
  CheckCircle,
  Copy,
  AlertTriangle,
  Zap,
  Terminal,
} from "lucide-react";

const prerequisites = [
  {
    icon: Container,
    title: "Docker 20.10+",
    description: "Docker and Docker Compose 2.0+ (for platform deployment)",
  },
  {
    icon: CheckCircle,
    title: "4GB RAM",
    description: "At least 4GB RAM available for containers (platform only)",
  },
  {
    icon: Download,
    title: "Git",
    description: "For cloning the repository",
  },
  {
    icon: Terminal,
    title: "Go 1.21+",
    description: "Required for building the CLI tool",
  },
];

const deploymentOptions = [
  {
    id: "docker",
    title: "Docker Compose",
    subtitle: "Recommended",
    description: "Get running locally in under 5 minutes",
    badge: { text: "Recommended", variant: "default" as const },
    icon: Container,
    code: `# Clone the repository
git clone https://github.com/alexanderwanyoike/the0.git
cd the0

# Start all services
cd docker
make up

# Access the platform
open http://localhost:3001  # Frontend
open http://localhost:3000  # API
open http://localhost:9001  # MinIO Console (admin/the0password)`,
  },
  {
    id: "kubernetes",
    title: "Kubernetes (Minikube)",
    subtitle: "Experimental",
    description: "Single command deployment with local endpoints",
    badge: { text: "Experimental", variant: "secondary" as const },
    icon: Zap,
    code: `# Navigate to k8s directory
cd k8s

# Single command deployment with local endpoints (experimental)
make minikube-up
make setup-hosts

# Note: Kubernetes deployment is highly experimental and may not work properly`,
  },
  {
    id: "cli",
    title: "CLI Tool Only",
    subtitle: "Development",
    description: "Install just the CLI for local development",
    badge: { text: "CLI Only", variant: "outline" as const },
    icon: Terminal,
    code: `# Clone the repository
git clone https://github.com/alexanderwanyoike/the0.git
cd the0/cli

# Build and install the CLI
make install

# Configure API endpoint for local deployments
# For Docker Compose:
export THE0_API_URL=http://localhost:3000
# For Kubernetes:
export THE0_API_URL=http://api.the0.local:3000

# Verify installation and authenticate
the0 --help
the0 auth login

# Usage examples
the0 bot list`,
  },
];

export function QuickStartSection() {
  const [activeOption, setActiveOption] = useState("docker");
  const [copiedCode, setCopiedCode] = useState(false);

  const copyToClipboard = async (text: string) => {
    try {
      await navigator.clipboard.writeText(text);
      setCopiedCode(true);
      setTimeout(() => setCopiedCode(false), 2000);
    } catch (err) {
      console.error("Failed to copy text: ", err);
    }
  };

  const activeDeployment = deploymentOptions.find(
    (option) => option.id === activeOption,
  );

  return (
    <section id="quick-start" className="py-16 md:py-24">
      <div className="container max-w-6xl">
        <div className="text-center mb-16">
          <h2 className="text-3xl font-bold mb-4">Get Started in Minutes</h2>
          <p className="text-xl text-muted-foreground max-w-2xl mx-auto">
            Get the0 running locally in under 5 minutes with Docker Compose or
            try our experimental Kubernetes deployment.
          </p>
        </div>

        {/* Prerequisites */}
        <div className="mb-16">
          <h3 className="text-xl font-semibold text-center mb-8">
            Prerequisites
          </h3>
          <div className="grid md:grid-cols-2 lg:grid-cols-4 gap-6 max-w-6xl mx-auto">
            {prerequisites.map((req, index) => (
              <Card key={index} className="text-center">
                <CardContent className="p-6">
                  <req.icon className="h-10 w-10 mx-auto mb-3 text-primary" />
                  <h4 className="font-semibold mb-2">{req.title}</h4>
                  <p className="text-sm text-muted-foreground">
                    {req.description}
                  </p>
                </CardContent>
              </Card>
            ))}
          </div>
        </div>

        {/* Deployment Options */}
        <div className="mb-12">
          <h3 className="text-xl font-semibold text-center mb-8">
            Choose Your Deployment Method
          </h3>
          <div className="flex justify-center mb-8">
            <div className="inline-flex bg-muted rounded-lg p-1">
              {deploymentOptions.map((option) => (
                <button
                  key={option.id}
                  onClick={() => setActiveOption(option.id)}
                  className={`flex items-center space-x-2 px-4 py-2 rounded-md text-sm font-medium transition-all ${
                    activeOption === option.id
                      ? "bg-background text-foreground shadow-sm"
                      : "text-muted-foreground hover:text-foreground"
                  }`}
                >
                  <option.icon className="h-4 w-4" />
                  <span>{option.title}</span>
                  <Badge variant={option.badge.variant} className="ml-2">
                    {option.badge.text}
                  </Badge>
                </button>
              ))}
            </div>
          </div>
        </div>

        {/* Active Deployment Instructions */}
        {activeDeployment && (
          <div className="max-w-4xl mx-auto mb-12">
            <Card>
              <CardHeader>
                <div className="flex items-center justify-between">
                  <div className="flex items-center space-x-3">
                    <activeDeployment.icon className="h-6 w-6 text-primary" />
                    <div>
                      <CardTitle className="flex items-center gap-2">
                        {activeDeployment.title}
                        <Badge variant={activeDeployment.badge.variant}>
                          {activeDeployment.badge.text}
                        </Badge>
                      </CardTitle>
                      <p className="text-sm text-muted-foreground mt-1">
                        {activeDeployment.description}
                      </p>
                    </div>
                  </div>
                  <Button
                    variant="outline"
                    size="sm"
                    onClick={() => copyToClipboard(activeDeployment.code)}
                  >
                    {copiedCode ? (
                      <CheckCircle className="h-4 w-4 text-green-600" />
                    ) : (
                      <Copy className="h-4 w-4" />
                    )}
                  </Button>
                </div>
              </CardHeader>
              <CardContent>
                <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm leading-relaxed">
                  <code>{activeDeployment.code}</code>
                </pre>
                {activeDeployment.id === "kubernetes" && (
                  <div className="flex items-start gap-2 mt-4 p-3 bg-amber-50 dark:bg-amber-900/10 border border-amber-200 dark:border-amber-900/20 rounded-lg">
                    <AlertTriangle className="h-4 w-4 text-amber-600 dark:text-amber-400 mt-0.5 flex-shrink-0" />
                    <p className="text-sm text-amber-800 dark:text-amber-200">
                      <strong>Experimental:</strong> Kubernetes deployment is
                      highly experimental and may not work properly. Use Docker
                      Compose for the best experience.
                    </p>
                  </div>
                )}
              </CardContent>
            </Card>
          </div>
        )}

        {/* Future Note */}
        <div className="text-center mt-8">
          <p className="text-muted-foreground">
            Cloud deployments will be available in the future.
          </p>
        </div>
      </div>
    </section>
  );
}
