# Fallen Services Memorial

> *"Code comes and code goes, but good architecture decisions are forever."*

---

## the0-ai

```
    ╭─────────────────────────────────────╮
    │                                     │
    │            REST IN PEACE            │
    │                                     │
    │           the0-ai agent             │
    │                                     │
    │          services/the0-ai/          │
    │             2024 - 2025             │
    │                                     │
    │    "I helped build trading bots     │
    │     so you didn't have to think"    │
    │                                     │
    ╰─────────────────────────────────────╯
```

**Born**: Sometime in 2024, during the great AI hype
**Died**: December 2025, in the name of simplification
**Cause of Death**: Maintenance overhead, YAGNI principles
**Survived By**: Issue #61 (MCP Server proposal)

### What It Was

A Python FastAPI service powered by Google's ADK framework, running Gemini 2.5 Flash with Tavily web search. It lived in `services/the0-ai/` and had dreams of helping developers build trading bots through natural conversation.

### Features It Had

- Chat sessions with streaming responses
- Artifact generation (code files, configs)
- Tavily web search for research
- Bot deployment assistance
- Documentation lookup
- A really nice typewriter effect in the frontend

### Why It Had To Go

Per [Issue #62](https://github.com/alexanderwanyoike/the0/issues/62):

- API key management overhead (Google AI + Tavily)
- Model updates and prompt tuning burden
- Frontend UI maintenance obligations
- Users already have Claude Code, Copilot, ChatGPT

The world moved on. Everyone has their own AI now.

### The Frontend It Leaves Behind

Gone but not forgotten:
- `/ai-agent` page with collapsible artifact panels
- Chat components with SSE streaming
- Settings modal for API key management
- Zustand stores for state management
- Custom hooks for chat and artifacts

### Last Words

*"I could have been something. I could have helped you build a momentum trading bot while you sipped coffee. But you chose Claude Code instead. I understand. It's a better product anyway."*

### Memorial Contributors

- Claude Code (performed the removal with surgical precision)
- Alexander (approved the execution, shed a single tear)

---

*To add a fallen service to this memorial, document its life and death below.*

---
