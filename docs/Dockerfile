# Build stage
FROM dhi.io/node:22-alpine3.22-dev AS builder

# Install git (required by VitePress for last updated timestamps)
RUN apk add --no-cache git

WORKDIR /app

# Copy package files
COPY package.json yarn.lock* ./

# Install dependencies
RUN yarn install --frozen-lockfile || yarn install

# Copy source files
COPY . .

# Build VitePress
RUN yarn build

# Production stage
FROM dhi.io/nginx:1.30-alpine3.23 AS production

# Copy built files to nginx
COPY --from=builder /app/.vitepress/dist /usr/share/nginx/html

# Copy nginx configuration
COPY nginx.conf /etc/nginx/conf.d/default.conf

USER 0

RUN mkdir -p /var/cache/nginx /var/run/nginx \
    && touch /var/run/nginx.pid \
    && chown -R 1000:1000 /var/cache/nginx /var/run/nginx /var/run/nginx.pid

USER 1000:1000

EXPOSE 8080
