# Build stage
FROM node:22-alpine AS builder

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
FROM nginx:alpine AS production

# Copy built files to nginx
COPY --from=builder /app/.vitepress/dist /usr/share/nginx/html

# Copy nginx configuration
COPY nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
