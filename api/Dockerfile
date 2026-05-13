# Use Docker Hardened Images for build and runtime stages.
FROM dhi.io/node:20-alpine3.22-dev AS base

# Set working directory
WORKDIR /app

# Install dependencies for building (needed for some native modules)
FROM base AS deps
RUN apk add --no-cache libc6-compat python3 make g++
COPY package.json yarn.lock ./
RUN yarn install --frozen-lockfile

# Build stage
FROM base AS builder
COPY --from=deps /app/node_modules ./node_modules
COPY . .

RUN yarn build
RUN mkdir -p uploads/custom-bots

# Production stage
FROM dhi.io/node:20-alpine3.22 AS runner
WORKDIR /app

# Copy built application
COPY --from=builder --chown=1000:1000 /app/dist ./dist
COPY --from=builder --chown=1000:1000 /app/src/database/migrations ./dist/src/database/migrations
COPY --from=builder --chown=1000:1000 /app/src/database/schema ./src/database/schema
COPY --from=builder --chown=1000:1000 /app/node_modules ./node_modules
COPY --from=builder --chown=1000:1000 /app/package.json ./package.json
COPY --from=builder --chown=1000:1000 /app/drizzle.config.ts ./drizzle.config.ts
COPY --from=builder --chown=1000:1000 /app/uploads ./uploads

USER 1000:1000

# Expose port
EXPOSE 3000

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
  CMD ["node", "-e", "require('http').get('http://127.0.0.1:3000/health', r => process.exit(r.statusCode === 200 ? 0 : 1)).on('error', () => process.exit(1))"]

# Start the application
CMD ["node", "dist/src/main.js"]
