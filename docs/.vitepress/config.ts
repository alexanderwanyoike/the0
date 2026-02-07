import { defineConfig } from 'vitepress'
import { withMermaid } from 'vitepress-plugin-mermaid'

export default withMermaid(defineConfig({
  title: 'the0',
  description: 'Open-source algorithmic trading platform documentation',

  cleanUrls: true,

  // Ignore dead links to external app routes (these point to the main app, not docs)
  ignoreDeadLinks: [
    /^\/dashboard/,
    /^\/custom-bots/,
    /^\/user-bots/,
    /^\/install-cli/,
    /^\/custom-bot-marketplace/,
    /^\/terminology\/marketplace/,
    /^\/terminology\/monitoring/,
    /local-development/,
    /^http:\/\/localhost/,
  ],

  head: [
    ['link', { rel: 'preconnect', href: 'https://fonts.googleapis.com' }],
    ['link', { rel: 'preconnect', href: 'https://fonts.gstatic.com', crossorigin: '' }],
    ['link', { href: 'https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap', rel: 'stylesheet' }],
    ['meta', { name: 'theme-color', content: '#1e5631' }],
  ],

  themeConfig: {
    siteTitle: 'the0',

    nav: [
      { text: 'Home', link: '/' },
      { text: 'Getting Started', link: '/welcome-to-the0' },
      { text: 'CLI', link: '/the0-cli/' },
      { text: 'Bot Development', link: '/custom-bot-development/' },
    ],

    sidebar: [
      {
        text: 'Getting Started',
        items: [
          { text: 'Welcome to the0', link: '/welcome-to-the0' },
        ]
      },
      {
        text: 'Terminology',
        collapsed: false,
        items: [
          { text: 'Overview', link: '/terminology/' },
          { text: 'Bots', link: '/terminology/bots' },
          { text: 'Custom Bots', link: '/terminology/custom-bots' },
          { text: 'Bot Deployment', link: '/terminology/bot-deployment' },
        ]
      },
      {
        text: 'the0 CLI',
        collapsed: false,
        items: [
          { text: 'Overview', link: '/the0-cli/' },
          { text: 'Installation', link: '/the0-cli/installation' },
          { text: 'Authentication', link: '/the0-cli/authentication' },
          { text: 'Bot Commands', link: '/the0-cli/bot-commands' },
          { text: 'Custom Bot Commands', link: '/the0-cli/custom-bot-commands' },
          { text: 'Secrets', link: '/the0-cli/secrets' },
        ]
      },
      {
        text: 'Language Quick Starts',
        collapsed: false,
        items: [
          { text: 'Python', link: '/custom-bot-development/python-quick-start' },
          { text: 'Node.js/TypeScript', link: '/custom-bot-development/nodejs-quick-start' },
          { text: 'Rust', link: '/custom-bot-development/rust-quick-start' },
          { text: 'C/C++', link: '/custom-bot-development/cpp-quick-start' },
          { text: 'C# (.NET 8)', link: '/custom-bot-development/csharp-quick-start' },
          { text: 'Scala 3', link: '/custom-bot-development/scala-quick-start' },
          { text: 'Haskell', link: '/custom-bot-development/haskell-quick-start' },
        ]
      },
      {
        text: 'Custom Bot Development',
        collapsed: false,
        items: [
          { text: 'Overview', link: '/custom-bot-development/' },
          { text: 'Development Overview', link: '/custom-bot-development/overview' },
          { text: 'Configuration', link: '/custom-bot-development/configuration' },
          { text: 'Bot Types', link: '/custom-bot-development/bot-types' },
          { text: 'State (Persistence)', link: '/custom-bot-development/state' },
          { text: 'Query Handlers', link: '/custom-bot-development/queries' },
          { text: 'Custom Frontends', link: '/custom-bot-development/custom-frontends' },
          { text: 'Metrics', link: '/custom-bot-development/metrics' },
          { text: 'Testing', link: '/custom-bot-development/testing' },
          { text: 'Deployment', link: '/custom-bot-development/deployment' },
        ]
      },
      {
        text: 'Integrations',
        collapsed: false,
        items: [
          { text: 'Overview', link: '/integrations/' },
          { text: 'MCP Server', link: '/integrations/mcp' },
        ]
      },
      {
        text: 'Runtime',
        collapsed: false,
        items: [
          { text: 'Overview', link: '/runtime/' },
          { text: 'Docker Mode', link: '/runtime/docker-mode' },
          { text: 'Kubernetes Mode', link: '/runtime/kubernetes-mode' },
        ]
      },
      {
        text: 'Platform Deployment',
        collapsed: false,
        items: [
          { text: 'Overview', link: '/deployment/' },
          { text: 'Docker Compose', link: '/deployment/docker-compose' },
          { text: 'Kubernetes', link: '/deployment/kubernetes' },
        ]
      }
    ],

    socialLinks: [
      { icon: 'github', link: 'https://github.com/alexanderwanyoike/the0' },
      { icon: 'discord', link: 'https://discord.gg/g5mp57nK' },
    ],

    footer: {
      message: 'Released under the Apache 2.0 License.',
      copyright: 'Copyright 2024-present the0'
    },

    search: {
      provider: 'local'
    },

    editLink: {
      pattern: 'https://github.com/alexanderwanyoike/the0/edit/main/docs/:path',
      text: 'Edit this page on GitHub'
    },

    // lastUpdated disabled - requires git history which isn't available in Docker builds
    // lastUpdated: {
    //   text: 'Last updated',
    //   formatOptions: { dateStyle: 'medium' }
    // },

    outline: {
      level: [2, 3],
      label: 'On this page'
    }
  },

  markdown: {
    lineNumbers: true,
    theme: {
      light: 'github-light',
      dark: 'github-dark'
    }
  },

  sitemap: {
    hostname: 'https://docs.the0.dev'
  }
}))
