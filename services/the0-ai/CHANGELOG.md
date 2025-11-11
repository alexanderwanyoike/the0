# Changelog

All notable changes to the0-ai service are documented in this file.

## [Unreleased]

### Added - Tavily Integration (2025-11-11)

#### 🔍 Production-Grade Web Search
- **Replaced** `search_web` (googlesearch-python) with **Tavily AI Search**
- AI-optimized search results with relevance scoring (0.0-1.0)
- AI-generated answer synthesis from search results
- Configurable search modes:
  - `basic`: Fast, cost-effective (~$0.005/search)
  - `advanced`: Comprehensive research (~$0.01/search)
- Free tier: 1,000 searches/month

#### 📝 Footnote-Style Citations
- All search results now use footnote references: `[^1]`, `[^2]`, etc.
- Citations listed in "References" section at end of results
- Cleaner inline text without URL clutter
- Format: `[^1]: [Title](URL)` (standard markdown)

#### ⚙️ API Key Management
- **New Endpoints:**
  - `POST /settings/tavily-api-key` - Configure API key
  - `GET /settings/tavily-api-key/status` - Check configuration
  - `DELETE /settings/tavily-api-key` - Remove API key
- Dual configuration: Environment variable (`TAVILY_API_KEY`) or database
- API key format validation (must start with `tvly-`)

#### 🧪 Testing Tools
- **New Make Commands:**
  - `make agent-cli` - Interactive CLI testing using ADK
  - `make agent-web` - Web UI testing using ADK
- 9 comprehensive unit tests for Tavily functionality
- All tests use mocked responses (no external API calls)

#### 📚 Documentation
- **README.md** - Complete project overview with Tavily features
- **TESTING.md** - Interactive testing guide
- **CITATIONS_EXAMPLE.md** - Citation format examples
- Updated **CLAUDE.md** - Architecture guide with Tavily integration

#### 🔧 Technical Details
- **Dependencies:**
  - Added `tavily-python==0.7.12`
- **Modified Files:**
  - `the0/tools/web_browser.py` - New `tavily_search()` function
  - `api/main.py` - Tavily API key endpoints
  - `the0/agent.py` - Updated tool registration and instructions
  - `requirements.txt` - Added Tavily SDK
- **Tests:**
  - 103 total tests passing
  - 9 Tavily-specific unit tests
  - Comprehensive error handling coverage

#### 🎯 Agent Enhancements
- Agent instructed to use footnote citations in responses
- Better source attribution for web research
- Professional citation format matching academic standards
- References section always included with search results

### Technical Improvements
- Async implementation with `AsyncTavilyClient`
- Comprehensive error handling:
  - Rate limiting with helpful messages
  - Authentication errors
  - Timeouts (30s default)
  - No results handling
  - Import errors
- Database fallback for API key retrieval
- Markdown formatting with proper structure

## Previous Versions

### Initial Release
- Google ADK integration (Gemini 2.5 Flash)
- FastAPI REST API
- PostgreSQL session management
- MinIO artifact storage
- Trading bot creation and deployment
- Documentation browsing
- Web page browsing (via browse_url)
- Basic web search (googlesearch-python) - now replaced with Tavily

---

**Note:** This changelog follows [Keep a Changelog](https://keepachangelog.com/) format.
