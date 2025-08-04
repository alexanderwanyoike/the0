# Use Node.js 20 as specified in package.json
FROM node:20-alpine AS base

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

# Generate database migrations and build the application
RUN yarn db:generate
RUN yarn build

# Production stage
FROM base AS runner
RUN apk add --no-cache dumb-init

# Create non-root user
RUN addgroup --system --gid 1001 nodejs
RUN adduser --system --uid 1001 nestjs

# Copy built application
COPY --from=builder --chown=nestjs:nodejs /app/dist ./dist
COPY --from=builder --chown=nestjs:nodejs /app/src/database/migrations ./dist/src/database/migrations
COPY --from=builder --chown=nestjs:nodejs /app/src/database/schema ./src/database/schema
COPY --from=builder --chown=nestjs:nodejs /app/node_modules ./node_modules
COPY --from=builder --chown=nestjs:nodejs /app/package.json ./package.json
COPY --from=builder --chown=nestjs:nodejs /app/drizzle.config.ts ./drizzle.config.ts

# Create uploads directory
RUN mkdir -p uploads/custom-bots && chown -R nestjs:nodejs uploads

USER nestjs

# Expose port
EXPOSE 3000

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD node dist/src/main.js || exit 1

# Start the application
CMD ["dumb-init", "node", "dist/src/main.js"]