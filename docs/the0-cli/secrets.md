---
title: "Secrets & Private Dependencies"
description: "Configure access to private GitHub and PyPI repositories"
tags: ["cli", "secrets", "github", "pypi", "private-repos"]
order: 5
---

# Secrets & Private Dependencies

When deploying custom bots that depend on private GitHub repositories or private PyPI packages, you need to configure secrets to authenticate during the vendoring process.

---

## Overview

During `the0 custom-bot deploy`, your bot's dependencies are vendored (installed into a `vendor/` directory) inside a Docker container. If your `requirements.txt` includes private packages, the CLI needs credentials to access them.

---

## GitHub Private Repositories

### Supported URL Formats

The CLI supports private GitHub dependencies in these formats:

```txt
# SSH format
git+ssh://git@github.com/org/private-repo.git

# Git@ format
git+git@github.com:org/private-repo.git

# HTTPS format
git+https://github.com/org/private-repo.git
```

### Setting Up GITHUB_TOKEN

1. **Create a Personal Access Token (PAT)**

   Go to [GitHub Settings > Developer settings > Personal access tokens](https://github.com/settings/tokens) and create a token with `repo` scope.

2. **Set the environment variable**

   ```bash
   export GITHUB_TOKEN="ghp_your_token_here"
   ```

3. **Deploy your bot**

   ```bash
   the0 custom-bot deploy
   ```

The CLI automatically:
- Installs git in the vendoring container
- Rewrites git URLs to use HTTPS with your token
- Authenticates all GitHub requests

### Example requirements.txt

```txt
# Public packages
numpy>=1.24.0
pandas>=2.0.0

# Private GitHub package (any format works)
git+ssh://git@github.com/myorg/my-private-lib.git@v1.0.0
git+https://github.com/myorg/another-private-lib.git@main

# Private package with egg name
git+ssh://git@github.com/myorg/trading-utils.git#egg=trading-utils
```

---

## Private PyPI Repositories

### Setting Up PIP_INDEX_URL

For private PyPI servers (e.g., Artifactory, AWS CodeArtifact, private PyPI):

```bash
export PIP_INDEX_URL="https://username:password@your-private-pypi.com/simple/"
```

Or for extra index URLs alongside public PyPI:

```bash
export PIP_EXTRA_INDEX_URL="https://username:password@your-private-pypi.com/simple/"
```

### AWS CodeArtifact Example

```bash
# Get auth token
export CODEARTIFACT_AUTH_TOKEN=$(aws codeartifact get-authorization-token \
  --domain your-domain \
  --query authorizationToken \
  --output text)

# Set pip index
export PIP_INDEX_URL="https://aws:${CODEARTIFACT_AUTH_TOKEN}@your-domain-123456789.d.codeartifact.us-east-1.amazonaws.com/pypi/your-repo/simple/"

# Deploy
the0 custom-bot deploy
```

---

## Multiple Secrets

You can combine multiple secrets for complex dependency setups:

```bash
# GitHub for private repos
export GITHUB_TOKEN="ghp_xxx"

# Private PyPI for internal packages
export PIP_EXTRA_INDEX_URL="https://user:pass@pypi.internal.com/simple/"

# Deploy
the0 custom-bot deploy
```

---

## Security Best Practices

### Don't Commit Secrets

Never commit tokens to version control. Use:

- Environment variables
- Secret managers (AWS Secrets Manager, HashiCorp Vault)
- CI/CD secret storage

### Use Short-Lived Tokens

- GitHub PATs can have expiration dates
- CodeArtifact tokens expire after 12 hours by default
- Rotate tokens regularly

### Minimal Scope

- GitHub: Only grant `repo` scope (read access to private repos)
- PyPI: Use read-only credentials when possible

### CI/CD Integration

Example GitHub Actions workflow:

```yaml
jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Deploy bot
        env:
          GITHUB_TOKEN: ${{ secrets.PRIVATE_REPO_TOKEN }}
          THE0_API_KEY: ${{ secrets.THE0_API_KEY }}
        run: |
          the0 auth login --api-key $THE0_API_KEY
          the0 custom-bot deploy
```

---

## Troubleshooting

### "Permission denied (publickey)"

Git is trying to use SSH but no key is available in the container.

**Solution**: Set `GITHUB_TOKEN` - the CLI will rewrite SSH URLs to HTTPS.

```bash
export GITHUB_TOKEN="ghp_xxx"
the0 custom-bot deploy
```

### "Repository not found"

Either the repo doesn't exist or your token lacks access.

**Solutions**:
- Verify the repository URL is correct
- Ensure your token has `repo` scope
- Check you have access to the repository

### "Could not find a version that satisfies the requirement"

The private package can't be found.

**Solutions**:
- Verify `PIP_INDEX_URL` or `PIP_EXTRA_INDEX_URL` is set
- Check the package name and version exist in your private index
- Ensure credentials are correct

### Verify Token Works

Test your GitHub token:

```bash
curl -H "Authorization: token $GITHUB_TOKEN" \
  https://api.github.com/repos/org/private-repo
```

Test your PyPI index:

```bash
pip install --index-url $PIP_INDEX_URL your-private-package
```

---

## Related Documentation

- [Custom Bot Commands](./custom-bot-commands) - Deploy custom bots
- [Authentication](./authentication) - CLI authentication setup
