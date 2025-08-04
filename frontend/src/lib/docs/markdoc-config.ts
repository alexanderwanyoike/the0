import { Config } from '@markdoc/markdoc';

export const config: Config = {
  tags: {
    button: {
      render: 'button',
      attributes: {
        variant: {
          type: String,
          default: 'default',
          matches: [
            'default',
            'destructive',
            'outline',
            'secondary',
            'ghost',
            'link',
          ],
        },
        size: {
          type: String,
          default: 'default',
          matches: ['default', 'sm', 'lg', 'icon'],
        },
        href: { type: String },
        disabled: { type: Boolean, default: false },
      },
    },
    alert: {
      render: 'div',
      attributes: {
        variant: {
          type: String,
          default: 'default',
          matches: ['default', 'destructive'],
        },
      },
    },
    'alert-title': {
      render: 'h4',
      attributes: {},
    },
    'alert-description': {
      render: 'div',
      attributes: {},
    },
    badge: {
      render: 'span',
      attributes: {
        variant: {
          type: String,
          default: 'default',
          matches: ['default', 'secondary', 'destructive', 'outline'],
        },
      },
    },
    card: {
      render: 'div',
      attributes: {},
    },
    'card-header': {
      render: 'div',
      attributes: {},
    },
    'card-title': {
      render: 'h3',
      attributes: {},
    },
    'card-description': {
      render: 'p',
      attributes: {},
    },
    'card-content': {
      render: 'div',
      attributes: {},
    },
  },
  nodes: {
    // Use existing highlight.js integration for code blocks
    fence: {
      render: 'pre',
      attributes: {
        language: {
          type: String,
        },
      },
    },
  },
};
