# the0

<div align="center">

**🚀 An Open-Source Algorithmic Trading Platform**

*Create, deploy, and manage trading bots with ease*

[![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Docker](https://img.shields.io/badge/Docker-Ready-2496ED?logo=docker)](https://www.docker.com/)
[![Kubernetes](https://img.shields.io/badge/Kubernetes-Soon-FFA500?logo=kubernetes)](https://kubernetes.io/)
[![Python](https://img.shields.io/badge/Python-3.11+-3776AB?logo=python)](https://www.python.org/)
[![JavaScript](https://img.shields.io/badge/JavaScript-Node.js%2020+-F7DF1E?logo=javascript)](https://nodejs.org/)

</div>

---

## 🎯 What is the0?

**the0** is a comprehensive algorithmic trading platform that empowers developers and traders to create, deploy, and manage trading bots across multiple markets. Whether you're building a simple Dollar Cost Averaging (DCA) strategy or complex multi-asset arbitrage algorithms, the0 provides the infrastructure and tools you need.

> ⚠️ **Early Development**: the0 is currently in active development and not yet production ready. We're building towards a stable release - contributions and feedback are welcome!

### ✨ Key Features

- 🤖 **Custom Bot Development** - Build bots in Python or JavaScript with any libraries you prefer
- 📊 **Advanced Backtesting** - Test strategies with historical data before going live
- ⚡ **Real-time Execution** - Deploy scheduled or continuous trading bots
- 🐳 **Docker Ready** - Easy deployment with Docker Compose
- 📈 **Analytics Dashboard** - Monitor performance and track metrics
- 🌐 **Exchange Agnostic** - Design your bots to work with any trading platform

---

## 🚀 Quick Start

Get the0 running locally in under 5 minutes:

### Prerequisites

- **Docker** 20.10+ and **Docker Compose** 2.0+
- At least 4GB RAM available for containers
- **Git** for cloning the repository

### Option 1: Docker Compose (Recommended)

```bash
# Clone the repository
git clone https://github.com/yourusername/the0.git
cd the0

# Start all services
cd docker
make up

# Access the platform
open http://localhost:3001  # Frontend
open http://localhost:3000  # API
open http://localhost:9001  # MinIO Console (admin/the0password)
```

### Option 2: Kubernetes (Highly Experimental)

```bash
# Navigate to k8s directory
cd k8s

# Single command deployment with local endpoints (experimental)
make minikube

# Note: Kubernetes deployment is highly experimental and may not work properly
```

### Option 3: Development Mode

```bash
# Start infrastructure only
cd docker
make infrastructure

# Run services locally
cd api && yarn dev      # API server
cd frontend && yarn dev # Frontend
cd runtime && go run cmd/app/main.go  # Runtime
```

---

## 🏗️ Architecture

the0 is built as a comprehensive microservices platform that enables algorithmic trading bot development and execution:

```mermaid
C4Container
    title Container Diagram - the0 Algorithmic Trading Platform
    
    Person_Ext(developer, "Bot Developer", "Creates and tests trading bots")
    Person_Ext(trader, "Trader", "Deploys and monitors trading bots")

    System_Boundary(the0_platform, "the0 Platform") {
        Container(frontend, "Web Dashboard", "Next.js 15, React 19", "Bot management, real-time monitoring, documentation")
        Container(cli, "CLI Tool", "Go, Cobra", "Local bot development, deployment commands")
        Container(api, "API Server", "NestJS, TypeScript", "REST API, authentication, orchestration")
        
        System_Boundary(runtime_services, "Runtime Services") {
            Container(bot_runner, "Bot Runner", "Go, gRPC", "Real-time bot execution with master-worker pattern")
            Container(backtest_runner, "Backtest Runner", "Go, gRPC", "Historical strategy testing and validation")  
            Container(bot_scheduler, "Bot Scheduler", "Go, gRPC", "Cron-based scheduled bot execution")
        }
        
        Container(security_analyzer, "Security Analyzer", "Python, YARA", "Automated bot code security analysis")
        Container(ai_service, "AI Assistant", "Python, FastAPI", "AI-powered bot development helper")
        
        ContainerDb(postgres, "PostgreSQL", "Database", "Users, bot definitions, auth tokens")
        ContainerDb(mongodb, "MongoDB", "Database", "Runtime state, job queues, execution logs")
        Container(nats, "NATS JetStream", "Message Broker", "Event streaming, service coordination")
        Container(minio, "MinIO", "Object Storage", "Bot code, logs, backtest results")
    }

    Rel(developer, frontend, "Manages bots", "HTTPS")
    Rel(developer, cli, "Develops locally", "CLI")
    Rel(trader, frontend, "Monitors performance", "HTTPS")
    
    Rel(frontend, api, "API calls", "REST/HTTP + JWT")
    Rel(cli, api, "Bot operations", "REST/HTTP + API Key")
    
    Rel(api, postgres, "User data", "SQL")
    Rel(api, nats, "Events", "NATS Protocol") 
    Rel(api, minio, "File storage", "S3 API")
    
    Rel(bot_runner, mongodb, "Execution state", "MongoDB")
    Rel(backtest_runner, mongodb, "Job queues", "MongoDB")
    Rel(bot_scheduler, mongodb, "Schedules", "MongoDB")
    
    Rel(nats, bot_runner, "Bot events", "NATS")
    Rel(nats, backtest_runner, "Backtest events", "NATS")
    Rel(nats, bot_scheduler, "Schedule events", "NATS")
    Rel(nats, security_analyzer, "Analysis events", "NATS")
    
    Rel(bot_runner, minio, "Logs", "S3 API")
    Rel(backtest_runner, minio, "Results", "S3 API")
    Rel(security_analyzer, minio, "Code analysis", "S3 API")
    
    Rel(frontend, api, "Real-time updates", "SSE")
    
    UpdateElementStyle(the0_platform, $bgColor="#e8f4fd")
    UpdateElementStyle(runtime_services, $bgColor="#fff8e1")
    UpdateElementStyle(frontend, $bgColor="#e1f5fe")
    UpdateElementStyle(api, $bgColor="#f3e5f5") 
    UpdateElementStyle(cli, $bgColor="#e8f5e8")
```

### How It Works

**🌐 Web Dashboard** - Next.js frontend for bot management, real-time monitoring, and comprehensive documentation system

**🛠️ CLI Tool** - Go-based command-line interface for local bot development, testing, and deployment automation

**🚀 API Server** - NestJS backend providing REST APIs, JWT authentication, and event orchestration across all services

**⚙️ Runtime Services** - Specialized Go microservices using master-worker patterns for different execution models:
- **Bot Runner**: Real-time trading bot execution
- **Backtest Runner**: Historical strategy validation  
- **Bot Scheduler**: Cron-based scheduled execution

**🔍 Security Analyzer** - Python service with YARA rules for automated security analysis of user-submitted bot code

**🤖 AI Assistant** - FastAPI service providing AI-powered bot development assistance and code generation. Standalone application for now, but will be integrated into the frontend in the future.

**💾 Data Architecture** - Multi-database approach:
- **PostgreSQL**: User accounts, bot definitions, authentication
- **MongoDB**: Runtime state, job queues, execution logs
- **MinIO**: Bot code storage, logs, backtest results
- **NATS JetStream**: Event streaming and service coordination

### Key Benefits

- **🔒 Secure**: Each bot runs in isolation with basic security checks
- **⚡ Fast**: Real-time execution with live market data
- **📈 Scalable**: Automatically handles multiple bots and users
- **🛡️ Reliable**: Robust error handling and monitoring

---

## 🤖 Bot Development

### Framework Agnostic Approach

the0 doesn't lock you into specific libraries or frameworks. Create bots using:

- **Python 3.11+** with any PyPI packages (pandas, numpy, ccxt, etc.)
- **JavaScript/Node.js 20+** with any npm packages
- **Open Standards**: YAML configuration, JSON Schema validation

### Example: Simple DCA Bot

```python
from typing import Dict, Any
from alpaca.trading.client import TradingClient

def main(id: str, config: Dict[str, Any]) -> Dict[str, Any]:
    """Dollar Cost Averaging bot - buys a fixed amount regularly"""
    
    # Initialize trading client
    client = TradingClient(
        api_key=config["api_key"],
        secret_key=config["secret_key"],
        paper=config.get("paper", True)
    )
    
    # Calculate and execute purchase
    symbol = config["symbol"]
    amount = config["amount"]
    
    # Place market buy order
    order = client.submit_order(
        symbol=symbol,
        notional=amount,
        side=OrderSide.BUY,
        type=OrderType.MARKET,
        time_in_force=TimeInForce.DAY
    )
    
    return {
        "status": "success",
        "message": f"Purchased ${amount} of {symbol}",
        "order_id": order.id
    }
```

### Bot Types

- **📅 Scheduled Bots** - Run on cron schedules (daily, weekly, monthly)
- **⚡ Real-time Bots** - Continuous execution with live data feeds

---

## 📚 Documentation

### Getting Started
- [Welcome to the0](frontend/src/docs/welcome-to-the0.md) - Platform overview
- [Custom Bot Development](frontend/src/docs/custom-bot-development/) - Build your first bot
- [Quick Start Guide](frontend/src/docs/custom-bot-development/quick-start-guide.md) - 15-minute DCA bot tutorial

### Deployment Guides
- [Docker Setup](docker/README.md) - Local development environment
- [Kubernetes Deployment](k8s/README.md) - Production deployment
- [CLI Installation](frontend/src/docs/the0-CLI/installation.md) - Command-line tools

### Development Resources
- [Bot Configuration](frontend/src/docs/custom-bot-development/configuration.md) - Configuration reference
- [Testing & Debugging](frontend/src/docs/custom-bot-development/testing.md) - Development best practices
- [Backtesting](frontend/src/docs/custom-bot-development/backtesting.md) - Strategy validation

---


## 🤝 Contributing

We welcome contributions from developers, traders, and AI enthusiasts! the0 is built by a community that values creativity and innovation.

### 🤖 AI-Friendly Development

We encourage the use of AI tools and agents in development:

- ✅ **AI Assistants Welcome** - Use Claude, ChatGPT, GitHub Copilot, or any AI tools you prefer
- ✅ **AI-Generated Code** - AI-written code is perfectly fine as long as it's well-tested
- ✅ **Creative Solutions** - We value innovative approaches and creative problem-solving
- ⚠️ **Quality First** - Ensure your code is properly tested, regardless of how it was created

### Ways to Contribute

- 🐛 **Bug Reports** - Found an issue? Let us know!
- 💡 **Feature Requests** - Have creative ideas for improvements?
- 🔧 **Code Contributions** - Submit pull requests (AI-assisted or not!)
- 📖 **Documentation** - Help improve our docs and examples
- 🤖 **Bot Templates** - Share innovative trading strategies and patterns
- 🎨 **Creative Ideas** - Think outside the box - we love unconventional approaches!

### Development Philosophy

- **🚀 Innovation Over Convention** - Creative solutions are encouraged
- **🧪 Experiment Freely** - Try new approaches and share your learnings  
- **🤝 Collaborate Openly** - Work with both humans and AI to build great software
- **✅ Test Thoroughly** - Well-tested code is good code, regardless of its origin
- **📚 Document Well** - Help others understand your creative solutions

### Getting Started

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/your-creative-idea`)
3. Build your solution (with or without AI assistance!)
4. Add comprehensive tests
5. Document your approach and any AI tools used
6. Submit a pull request with a clear description

We believe the best software comes from combining human creativity with AI capabilities. Don't hesitate to experiment and push boundaries!

---

## 📄 License

This project is licensed under the Apache License 2.0 - see the [LICENSE](LICENSE) file for details.

---

## 🆘 Support & Community

- 📧 **Support**: [support@the0.dev](mailto:support@the0.dev)
- 💬 **Discord**: [Join our community](https://discord.gg/the0-community)
- 📚 **Documentation**: [docs.the0.dev](https://docs.the0.dev)
- 🐙 **GitHub Issues**: [Report bugs or request features](https://github.com/yourusername/the0/issues)

---

<div align="center">

**Built with ❤️ by AlphaNeuron**

[Website](https://the0.dev) • [Documentation](https://docs.the0.dev) • [Discord](https://discord.gg/the0-community)

</div>