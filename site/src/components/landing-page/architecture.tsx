"use client";
import { Card, CardContent } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";
import {
  Globe,
  Terminal,
  Server,
  Cpu,
  Activity,
  Shield,
  ArrowRight,
  ArrowDown,
} from "lucide-react";

const services = [
  {
    id: "web",
    name: "Web Dashboard",
    description: "Bot management & monitoring",
    tech: "Next.js 15, React 19",
    icon: Globe,
    type: "client",
    color: "bg-blue-500",
    textColor: "text-blue-700",
    bgColor: "bg-blue-50 dark:bg-blue-950/20",
    borderColor: "border-blue-200 dark:border-blue-800",
  },
  {
    id: "cli",
    name: "CLI Tool",
    description: "Local development interface",
    tech: "Go, Cobra",
    icon: Terminal,
    type: "client",
    color: "bg-green-500",
    textColor: "text-green-700",
    bgColor: "bg-green-50 dark:bg-green-950/20",
    borderColor: "border-green-200 dark:border-green-800",
  },
  {
    id: "api",
    name: "API Server",
    description: "REST APIs & orchestration",
    tech: "NestJS, TypeScript",
    icon: Server,
    type: "api",
    color: "bg-purple-500",
    textColor: "text-purple-700",
    bgColor: "bg-purple-50 dark:bg-purple-950/20",
    borderColor: "border-purple-200 dark:border-purple-800",
  },
  {
    id: "bot-runner",
    name: "Bot Runner",
    description: "Real-time bot execution",
    tech: "Go, gRPC",
    icon: Cpu,
    type: "runtime",
    color: "bg-orange-500",
    textColor: "text-orange-700",
    bgColor: "bg-orange-50 dark:bg-orange-950/20",
    borderColor: "border-orange-200 dark:border-orange-800",
  },
  {
    id: "backtest-runner",
    name: "Backtest Runner",
    description: "Historical strategy testing",
    tech: "Go, gRPC",
    icon: Activity,
    type: "runtime",
    color: "bg-orange-500",
    textColor: "text-orange-700",
    bgColor: "bg-orange-50 dark:bg-orange-950/20",
    borderColor: "border-orange-200 dark:border-orange-800",
  },
  {
    id: "scheduler",
    name: "Bot Scheduler",
    description: "Cron-based execution",
    tech: "Go, gRPC",
    icon: Activity,
    type: "runtime",
    color: "bg-orange-500",
    textColor: "text-orange-700",
    bgColor: "bg-orange-50 dark:bg-orange-950/20",
    borderColor: "border-orange-200 dark:border-orange-800",
  },
  {
    id: "security",
    name: "Security Analyzer",
    description: "Code analysis & validation",
    tech: "Python, YARA",
    icon: Shield,
    type: "service",
    color: "bg-pink-500",
    textColor: "text-pink-700",
    bgColor: "bg-pink-50 dark:bg-pink-950/20",
    borderColor: "border-pink-200 dark:border-pink-800",
  },
  {
    id: "ai-assistant",
    name: "AI Assistant",
    description: "Bot development & deployment help",
    tech: "FastAPI, Claude API",
    icon: Activity,
    type: "service",
    color: "bg-indigo-500",
    textColor: "text-indigo-700",
    bgColor: "bg-indigo-50 dark:bg-indigo-950/20",
    borderColor: "border-indigo-200 dark:border-indigo-800",
  },
];

const layers = [
  {
    title: "Client Layer",
    description: "User interfaces and development tools",
    services: services.filter((s) => s.type === "client"),
  },
  {
    title: "API Layer",
    description: "REST APIs and orchestration",
    services: services.filter((s) => s.type === "api"),
  },
  {
    title: "Runtime Services",
    description: "Core execution and processing services",
    services: services.filter((s) => s.type === "runtime"),
  },
  {
    title: "Supporting Services",
    description: "Security and analysis services",
    services: services.filter((s) => s.type === "service"),
  },
];

export function ArchitectureSection() {
  return (
    <section className="py-16 md:py-24">
      <div className="container max-w-7xl">
        <div className="text-center mb-16">
          <h2 className="text-3xl font-bold mb-4">
            Built for Scale and Reliability
          </h2>
          <p className="text-xl text-muted-foreground max-w-3xl mx-auto">
            Microservices architecture designed for high-performance trading bot
            execution with built-in security and monitoring.
          </p>
        </div>

        {/* C4-Style Architecture Diagram */}
        <div className="mb-16">
          <div className="grid grid-cols-1 lg:grid-cols-12 gap-6">
            {layers.map((layer, layerIndex) => (
              <div
                key={layerIndex}
                className={`${
                  layer.services.length === 1
                    ? "lg:col-span-2"
                    : "lg:col-span-3"
                }`}
              >
                <div className="mb-6">
                  <h3 className="text-sm font-semibold text-muted-foreground uppercase tracking-wide mb-2">
                    {layer.title}
                  </h3>
                  <p className="text-xs text-muted-foreground">
                    {layer.description}
                  </p>
                </div>
                <div className="space-y-4">
                  {layer.services.map((service) => (
                    <Card
                      key={service.id}
                      className={`${service.bgColor} ${service.borderColor} border-2 hover:shadow-lg transition-all duration-200`}
                    >
                      <CardContent className="p-4">
                        <div className="flex items-start space-x-3">
                          <div
                            className={`${service.color} p-2 rounded-lg flex-shrink-0`}
                          >
                            <service.icon className="h-4 w-4 text-white" />
                          </div>
                          <div className="flex-1 min-w-0">
                            <div className="flex items-center gap-2 mb-1">
                              <h4
                                className={`text-sm font-semibold ${service.textColor} dark:text-foreground`}
                              >
                                {service.name}
                              </h4>
                            </div>
                            <p className="text-xs text-muted-foreground mb-2">
                              {service.description}
                            </p>
                            <Badge
                              variant="secondary"
                              className="text-xs px-2 py-0.5"
                            >
                              {service.tech}
                            </Badge>
                          </div>
                        </div>
                      </CardContent>
                    </Card>
                  ))}
                </div>
              </div>
            ))}
          </div>

          {/* Flow Arrows */}
          <div className="flex justify-center my-8">
            <div className="hidden lg:flex items-center space-x-4 text-muted-foreground">
              <ArrowRight className="h-5 w-5" />
              <span className="text-sm">Data Flow</span>
              <ArrowRight className="h-5 w-5" />
            </div>
            <div className="lg:hidden flex flex-col items-center space-y-2 text-muted-foreground">
              <ArrowDown className="h-5 w-5" />
              <span className="text-sm">Data Flow</span>
              <ArrowDown className="h-5 w-5" />
            </div>
          </div>
        </div>

        {/* Key Architecture Benefits */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-8 max-w-5xl mx-auto">
          <div className="text-center">
            <div className="inline-flex items-center justify-center w-12 h-12 bg-blue-50 dark:bg-blue-950/20 text-blue-600 dark:text-blue-400 rounded-lg mb-4">
              <Shield className="h-6 w-6" />
            </div>
            <h3 className="text-lg font-semibold mb-2">Secure by Design</h3>
            <p className="text-muted-foreground">
              Built-in security analysis, isolated execution, and comprehensive
              audit logging.
            </p>
          </div>
          <div className="text-center">
            <div className="inline-flex items-center justify-center w-12 h-12 bg-orange-50 dark:bg-orange-950/20 text-orange-600 dark:text-orange-400 rounded-lg mb-4">
              <Activity className="h-6 w-6" />
            </div>
            <h3 className="text-lg font-semibold mb-2">High Performance</h3>
            <p className="text-muted-foreground">
              Optimized Go runtime services with event-driven architecture for
              low-latency trading.
            </p>
          </div>
          <div className="text-center">
            <div className="inline-flex items-center justify-center w-12 h-12 bg-purple-50 dark:bg-purple-950/20 text-purple-600 dark:text-purple-400 rounded-lg mb-4">
              <Cpu className="h-6 w-6" />
            </div>
            <h3 className="text-lg font-semibold mb-2">Auto Scaling</h3>
            <p className="text-muted-foreground">
              Customizable autoscaling based on the overall number of bots per worker. 
              Kubernetes-ready microservices adapt to your trading load.
            </p>
          </div>
        </div>
      </div>

      {/* Data Sources */}
      <div className="container mx-auto px-4 py-12">
        <div className="text-center mb-8">
          <h2 className="text-2xl font-bold mb-4">Data Infrastructure</h2>
          <p className="text-muted-foreground max-w-2xl mx-auto">
            Multi-database architecture optimized for different data types and access patterns.
          </p>
        </div>
        <div className="grid md:grid-cols-2 lg:grid-cols-4 gap-6">
          <div className="text-center">
            <div className="inline-flex items-center justify-center w-12 h-12 bg-teal-50 dark:bg-teal-950/20 text-teal-600 dark:text-teal-400 rounded-lg mb-4">
              <Server className="h-6 w-6" />
            </div>
            <h3 className="text-lg font-semibold mb-2">PostgreSQL</h3>
            <p className="text-muted-foreground text-sm">
              User accounts, bot definitions, and structured data storage.
            </p>
          </div>
          <div className="text-center">
            <div className="inline-flex items-center justify-center w-12 h-12 bg-green-50 dark:bg-green-950/20 text-green-600 dark:text-green-400 rounded-lg mb-4">
              <Activity className="h-6 w-6" />
            </div>
            <h3 className="text-lg font-semibold mb-2">MongoDB</h3>
            <p className="text-muted-foreground text-sm">
              Runtime state, execution logs, and dynamic bot data.
            </p>
          </div>
          <div className="text-center">
            <div className="inline-flex items-center justify-center w-12 h-12 bg-blue-50 dark:bg-blue-950/20 text-blue-600 dark:text-blue-400 rounded-lg mb-4">
              <Globe className="h-6 w-6" />
            </div>
            <h3 className="text-lg font-semibold mb-2">MinIO</h3>
            <p className="text-muted-foreground text-sm">
              S3-compatible storage for bot code, files, and artifacts.
            </p>
          </div>
          <div className="text-center">
            <div className="inline-flex items-center justify-center w-12 h-12 bg-purple-50 dark:bg-purple-950/20 text-purple-600 dark:text-purple-400 rounded-lg mb-4">
              <ArrowRight className="h-6 w-6" />
            </div>
            <h3 className="text-lg font-semibold mb-2">NATS JetStream</h3>
            <p className="text-muted-foreground text-sm">
              Event streaming and message coordination between services.
            </p>
          </div>
        </div>
      </div>
    </section>
  );
}
