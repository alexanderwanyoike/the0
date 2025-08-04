# the0 AI Agent Workbench

A modern React TypeScript frontend for the the0 AI agent that provides an intuitive workbench interface for building trading bots.

## Features

- 💬 **Interactive Chat**: Chat with the0 AI agent to describe your trading strategies
- 🗂️ **File Tree**: Browse generated bot files in an organized tree structure
- 🖥️ **Code Editor**: Monaco editor with syntax highlighting for viewing/editing code
- 🎨 **Clean UI**: Built with shadcn/ui components and Tailwind CSS
- 🔄 **Real-time Updates**: Live updates as the agent generates new files
- 🌙 **Dark/Light Theme**: Support for both light and dark themes

## Tech Stack

- **React 18** with TypeScript for type safety
- **Vite** for fast development and building
- **Tailwind CSS** for styling with the0's design system
- **shadcn/ui** for consistent UI components
- **Monaco Editor** for code editing with syntax highlighting
- **Zustand** for lightweight state management
- **TanStack Query** for API state management

## Getting Started

### Prerequisites

- Node.js 18+
- npm or yarn

### Installation

1. Install dependencies:

```bash
npm install
```

2. Start the development server:

```bash
npm run dev
```

3. Open your browser to `http://localhost:3000`

### Backend Setup

Make sure the the0 AI agent backend is running on `http://localhost:8000`. The frontend is configured to proxy API requests to the backend.

## Project Structure

```
src/
├── components/
│   ├── ui/                 # shadcn/ui components
│   ├── chat/               # Chat-related components
│   ├── artifacts/          # Artifacts panel components
│   ├── layout/             # Layout components
│   └── common/             # Shared components
├── hooks/                  # Custom React hooks
├── lib/                    # Utilities and configurations
├── services/               # API services
├── stores/                 # Zustand stores
├── types/                  # TypeScript type definitions
└── styles/                 # Global styles
```

## Available Scripts

- `npm run dev` - Start development server
- `npm run build` - Build for production
- `npm run preview` - Preview production build
- `npm run lint` - Run ESLint

## Configuration

The app uses Vite's proxy feature to forward API requests to the backend. You can modify the proxy configuration in `vite.config.ts`.

## Contributing

1. Follow the existing code style and patterns
2. Use TypeScript for all new code
3. Add appropriate types for new components and functions
4. Test your changes thoroughly

## License

MIT
