# the0-ai

AI-powered agent service for building and deploying automated trading bots on the the0 platform. Built with Google's Agent Development Kit (ADK), FastAPI, and Tavily AI search.

## Features

- 🤖 **Conversational AI Agent** - Natural language interface for bot creation
- 🔍 **AI-Optimized Web Search** - Powered by Tavily with relevance scoring and citations
- 📝 **Automatic Code Generation** - Creates complete trading bot implementations
- 🧪 **Backtesting Support** - Built-in strategy testing and validation
- 🚀 **One-Click Deployment** - Automated bot packaging and deployment
- 💾 **Session Management** - Persistent conversation history with PostgreSQL
- 📦 **Artifact Storage** - MinIO integration for bot file management

## Technology Stack

- **Framework**: FastAPI 0.116.1 + Uvicorn 0.35.0
- **AI Agent**: Google ADK 1.6.1 (Gemini 2.5 Flash)
- **Search**: Tavily Python SDK 0.7.12
- **Database**: PostgreSQL (asyncpg 0.30.0) + SQLAlchemy 2.0.41
- **Storage**: MinIO 7.2.8 (S3-compatible)
- **Testing**: Pytest 8.3.2
- **Python**: 3.11+

## Quick Start

### 1. Setup Environment

```bash
# Clone and navigate to the project
cd the0-ai

# Set up virtual environment and dependencies
make setup
```

### 2. Configure Environment Variables

Create a `.env` file:

```bash
# Database
DATABASE_URL=postgresql+asyncpg://user:password@localhost:5432/the0ai

# Google AI (Gemini)
GOOGLE_API_KEY=your-google-api-key

# Tavily Search (required for web search functionality)
TAVILY_API_KEY=tvly-your-tavily-api-key

# MinIO Storage
MINIO_ENDPOINT=localhost:9000
MINIO_ACCESS_KEY=minioadmin
MINIO_SECRET_KEY=minioadmin
AI_ARTIFACTS_BUCKET=ai-artifacts

# Optional: Documentation Service
DOCS_ENDPOINT=https://the0.dev/api/docs
```

**Get API Keys:**
- Google AI: https://makersuite.google.com/app/apikey
- Tavily (Free tier: 1,000 searches/month): https://tavily.com

### 3. Run the Service

```bash
# Development mode (with auto-reload)
make dev

# Or standard mode
make run-agent
```

API available at: http://localhost:8000

## Interactive Testing

Test the agent interactively using Google ADK's built-in tools:

### CLI Testing
```bash
make agent-cli
```
Chat with the agent directly in your terminal. Perfect for quick testing and development.

### Web UI Testing
```bash
make agent-web
```
Access the web interface at http://localhost:8000 with a visual chat UI.

**Example queries:**
- "Search for momentum trading strategies"
- "Help me build a bot that trades Bitcoin using RSI"
- "Find the latest Binance API documentation"

## Tavily Search Integration

The agent uses **Tavily AI Search** for production-grade web research:

### Features
- ✅ **AI-Generated Summaries** - Synthesized answers from search results
- ✅ **Relevance Scoring** - Each result rated 0.0-1.0
- ✅ **Footnote Citations** - Clean numbered references: `[^1]`, `[^2]`, etc.
- ✅ **Two Search Modes**:
  - `basic`: Fast, cost-effective (~$0.005/search)
  - `advanced`: Comprehensive research (~$0.01/search)

### Example Output

```markdown
According to recent research[^1], momentum strategies work best in trending markets.
The Binance API supports both REST and WebSocket connections[^2].

---

## References

[^1]: [Momentum Trading Strategies](https://example.com/momentum)
[^2]: [Binance API Documentation](https://binance-docs.github.io/)
```

### Cost Management
- **Free Tier**: 1,000 searches/month
- **Basic Search**: ~$0.005 per search (default)
- **Advanced Search**: ~$0.01 per search
- Upgrade at: https://tavily.com/pricing

## API Endpoints

### Health & Version
- `GET /health` - Service health check
- `GET /version` - Version information

### Chat
- `POST /chat` - Synchronous chat
- `POST /chat/stream` - Streaming chat (SSE)

### Session Management
- `GET /chat/sessions` - List sessions
- `GET /chat/sessions/{session_id}` - Get session with history
- `PUT /chat/sessions/{session_id}/title` - Update session title
- `DELETE /chat/sessions/{session_id}` - Delete session

### Artifacts
- `GET /artifacts/session/{session_id}` - List session artifacts
- `GET /artifacts/session/{session_id}/{filename}` - Get specific artifact
- `GET /artifacts/download` - Download all artifacts as ZIP

### Settings
- `POST /settings/api-key` - Set Google AI API key
- `GET /settings/api-key/status` - Check API key status
- `DELETE /settings/api-key` - Remove API key

**Tavily Settings:**
- `POST /settings/tavily-api-key` - Set Tavily API key
- `GET /settings/tavily-api-key/status` - Check Tavily configuration
- `DELETE /settings/tavily-api-key` - Remove Tavily key

## Development

### Code Quality

```bash
# Format code with Black
make format

# Lint with Flake8
make lint

# Run tests
make test

# Watch mode for tests
make test-watch
```

### Database Migrations

```bash
# Create new migration
alembic revision --autogenerate -m "description"

# Apply migrations
alembic upgrade head

# Rollback
alembic downgrade -1
```

### Testing Strategy

```bash
# Run all tests (103 tests)
make test

# Run specific test suite
pytest tests/api/ -v                           # API tests
pytest tests/the0/tools/ -v                    # Tool tests

# Run with coverage
pytest --cov=api --cov=the0 --cov-report=term-missing
```

**Test Coverage:**
- API endpoints
- Agent tools (Tavily search, documentation, artifacts, deployment)
- Database repositories
- Session management
- Error handling

## Project Structure

```
the0-ai/
├── api/                      # FastAPI application layer
│   ├── main.py              # REST endpoints
│   ├── agent_service.py     # Agent orchestration
│   ├── schemas.py           # Pydantic models
│   ├── database.py          # Database connection
│   ├── repositories.py      # Data access layer
│   ├── storage.py           # MinIO service
│   └── models/database.py   # SQLAlchemy models
├── the0/                    # Google ADK agent
│   ├── agent.py            # Agent definition
│   ├── agents/             # Multi-agent system
│   └── tools/              # Custom tools
│       ├── web_browser.py       # Tavily search + browsing
│       ├── save_artifact.py     # File creation
│       ├── documentation.py     # Docs API
│       └── deploy_bot.py        # Bot deployment
├── tests/                   # Test suite
├── alembic/                # Database migrations
├── requirements.txt        # Dependencies
├── Makefile               # Development commands
└── CLAUDE.md              # Architecture documentation
```

## Agent Capabilities

The agent can:
1. **Search the Web** - Uses Tavily to find current information
2. **Browse URLs** - Fetches and converts web pages to markdown
3. **Access Documentation** - Reads the0 platform docs
4. **Create Bot Files** - Generates complete bot implementations
5. **Backtest Strategies** - Creates backtesting configurations
6. **Deploy Bots** - Packages and prepares for deployment

### Workflow Example

```
User: "Help me build a momentum trading bot for Bitcoin"

Agent:
1. Searches for momentum trading strategies (Tavily)
2. Browses relevant documentation
3. Accesses the0 platform guides
4. Creates bot files:
   - main.py (strategy logic)
   - bot-config.yaml (configuration)
   - requirements.txt (dependencies)
   - README.md (documentation)
   - backtest.py (testing)
5. Provides implementation details with citations
```

## Docker Deployment

```bash
# Build image
docker build -t the0-ai .

# Run container
docker run -p 8000:8000 --env-file .env the0-ai
```

**Features:**
- Health checks every 30s
- Auto-migrations on startup
- Non-root user (appuser)
- Graceful shutdown

## Environment Variables Reference

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `DATABASE_URL` | Yes | - | PostgreSQL connection string |
| `GOOGLE_API_KEY` | Yes* | - | Google AI (Gemini) API key |
| `TAVILY_API_KEY` | Yes* | - | Tavily search API key |
| `MINIO_ENDPOINT` | Yes | - | MinIO server address |
| `MINIO_ACCESS_KEY` | Yes | - | MinIO access key |
| `MINIO_SECRET_KEY` | Yes | - | MinIO secret key |
| `AI_ARTIFACTS_BUCKET` | No | ai-artifacts | Bucket name for artifacts |
| `DOCS_ENDPOINT` | No | - | Documentation API URL |

*Can be set via API or environment variable

## Troubleshooting

### "Tavily API key not configured"

**Solution:**
```bash
# Option 1: Environment variable
echo "TAVILY_API_KEY=tvly-your-key" >> .env

# Option 2: API endpoint (while service is running)
curl -X POST http://localhost:8000/settings/tavily-api-key \
  -H "Content-Type: application/json" \
  -d '{"api_key": "tvly-your-key"}'
```

### Rate Limit Exceeded

**Problem:** Free tier limit (1,000 searches/month) reached

**Solution:**
- Wait until next month for free tier reset
- Upgrade at https://tavily.com/pricing
- Use `search_depth="basic"` to minimize costs

### Database Connection Failed

**Solution:**
```bash
# Check PostgreSQL is running
sudo systemctl status postgresql

# Verify DATABASE_URL format
postgresql+asyncpg://user:password@host:port/database
```

## Make Commands Reference

| Command | Description |
|---------|-------------|
| `make setup` | Set up venv and install dependencies |
| `make dev` | Run in development mode (auto-reload) |
| `make run-agent` | Run in production mode |
| `make agent-cli` | Interactive CLI testing (ADK) |
| `make agent-web` | Web UI testing (ADK) |
| `make test` | Run all tests |
| `make test-watch` | Run tests in watch mode |
| `make format` | Format code with Black |
| `make lint` | Lint code with Flake8 |
| `make clean` | Clean cache and artifacts |

## Contributing

1. Create feature branch
2. Write tests for new functionality
3. Run `make format && make lint && make test`
4. Ensure all tests pass
5. Update documentation
6. Submit PR with clear description

## Documentation

- **Architecture Guide**: See [CLAUDE.md](CLAUDE.md) for comprehensive architecture documentation
- **Testing Guide**: See [TESTING.md](TESTING.md) for interactive testing instructions
- **Citations Example**: See [CITATIONS_EXAMPLE.md](CITATIONS_EXAMPLE.md) for footnote citation format
- **Google ADK**: https://google.github.io/adk-docs/
- **Tavily API**: https://docs.tavily.com/
- **the0 Platform**: https://the0.dev/docs

## License

See project root for license information.

## Support

For issues and questions:
- GitHub Issues: https://github.com/alexanderwanyoike/the0/issues
- Documentation: https://the0.dev/docs
- Platform: https://the0.dev

---
