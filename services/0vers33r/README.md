# 0vers33r Security Analysis Service

A standalone microservice for analyzing custom trading bots for security vulnerabilities and malicious code patterns. This service replaces the original Firebase Cloud Function implementation with a scalable, cloud-native architecture.

## Overview

The 0vers33r service provides comprehensive security analysis for trading bots using:

- **YARA Rules**: Pattern-based detection of malicious code in Python and JavaScript
- **AI Analysis**: Advanced semantic analysis using Google Gemini AI
- **Multi-Runtime Support**: Handles both Python 3.11 and Node.js 20 bots
- **Event-Driven Architecture**: Integrates with NATS messaging for real-time processing

## Architecture

```
┌─────────────────┐    NATS Events    ┌─────────────────┐
│   the0 API      │◄──────────────────►│   0vers33r      │
│                 │                   │   Service       │
└─────────────────┘                   └─────────────────┘
         │                                     │
         │                                     │
         ▼                                     ▼
┌─────────────────┐                   ┌─────────────────┐
│   PostgreSQL    │                   │     MinIO       │
│   Database      │                   │   Storage       │
└─────────────────┘                   └─────────────────┘
```

## Event Flow

1. **Bot Submission**: User uploads bot via API
2. **Event Publishing**: API publishes `custom-bot.submitted` event
3. **Analysis Trigger**: 0vers33r receives event and starts analysis
4. **File Download**: Service downloads bot files from MinIO
5. **Security Scan**: YARA rules + AI analysis run on code
6. **Result Publishing**: Service publishes analysis result events
7. **Status Update**: API receives results and updates bot status

## Event Types

### Input Events
- `custom-bot.submitted`: New bot ready for analysis

### Output Events
- `custom-bot.approved`: Bot passed security checks
- `custom-bot.declined`: Bot failed security checks  
- `custom-bot.awaiting-human-review`: Bot requires manual review
- `custom-bot.analysis-failed`: Analysis encountered an error

## Analysis Process

### 1. Runtime Detection
- Automatically detects Python vs JavaScript bots
- Applies runtime-specific analysis rules and patterns

### 2. YARA Analysis
- Scans code files using 200+ security rules
- Detects malware, exploits, and suspicious patterns
- Provides threat scoring (0-5 scale)

### 3. AI Analysis (Optional)
- Uses Google Gemini for semantic code analysis
- Analyzes up to 800K characters of context
- Provides intelligent threat assessment

### 4. Result Aggregation
- Combines YARA and AI scores
- Determines final bot status
- Generates detailed analysis reports

## Deployment

### Docker Compose

The service is automatically included in the main the0 OSS docker-compose stack:

```bash
cd /path/to/the0/oss/docker
docker-compose up -d the0-analyzer
```

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `NATS_URL` | NATS server URL | `nats://localhost:4222` |
| `MINIO_ENDPOINT` | MinIO server endpoint | `localhost:9000` |
| `MINIO_ACCESS_KEY` | MinIO access key | `minioadmin` |
| `MINIO_SECRET_KEY` | MinIO secret key | `minioadmin` |
| `DATABASE_URL` | PostgreSQL connection string | Required |
| `GEMINI_API_KEY` | Google Gemini API key | Optional |
| `CUSTOM_BOTS_BUCKET` | MinIO bucket for bot files | `custom-bots` |

### Scaling

The service supports horizontal scaling with NATS queue groups:

```bash
docker-compose up -d --scale the0-analyzer=3
```

Multiple instances will automatically load balance analysis work.

## Monitoring

### Health Checks
- Built-in Docker health checks
- NATS connection monitoring
- Database connectivity validation

### Logging
- Structured JSON logging
- Analysis metrics and timing
- Error tracking and debugging

### Metrics
- Analysis success/failure rates
- Processing time per bot
- Threat detection statistics

## Security Features

### Malware Detection
- **Code Injection**: eval(), exec(), Function constructor
- **Reverse Shells**: Network connections, command execution
- **Credential Theft**: API key harvesting, keyloggers
- **System Destruction**: File deletion, system calls
- **Crypto Mining**: Mining pools, crypto libraries

### Trading Bot Specific
- **API Abuse**: Unauthorized trading actions
- **Pump & Dump**: Market manipulation patterns
- **Account Takeover**: Trading account compromise
- **Financial Theft**: Wallet draining, fee skimming

## Development

### Local Development

1. **Clone Repository**
   ```bash
   git clone <repo-url>
   cd the0/oss/services/0vers33r
   ```

2. **Install Dependencies**
   ```bash
   pip install -r requirements.txt
   ```

3. **Run Tests**
   ```bash
   python -m pytest tests/
   ```

4. **Start Service**
   ```bash
   export NATS_URL=nats://localhost:4222
   export DATABASE_URL=postgresql://user:pass@localhost:5432/the0
   python main.py
   ```

### Testing

The service includes comprehensive test suites:

- **Unit Tests**: Core analysis logic
- **Integration Tests**: NATS and database interactions
- **Security Tests**: Malware detection accuracy
- **Performance Tests**: Analysis speed and memory usage

```bash
# Run all tests
python -m pytest tests/ -v

# Run specific test categories
python -m pytest tests/test_analyzer.py -v
python -m pytest tests/test_ai_analysis.py -v
```

### Adding New Rules

1. **Create YARA Rule**
   ```yara
   rule NewThreat {
       meta:
           description = "Detects new threat pattern"
           severity = "high"
       strings:
           $pattern = "malicious_code_pattern"
       condition:
           $pattern
   }
   ```

2. **Add to Rule Directory**
   ```bash
   cp new_rule.yar yara_rules/python_suspicious.yar
   ```

3. **Test Rule**
   ```bash
   python -m pytest tests/test_rules.py::test_new_rule -v
   ```

## Migration from Firebase

The service maintains compatibility with the original Firebase Cloud Function while adding:

- **Better Scalability**: Horizontal scaling with NATS
- **Improved Reliability**: No cold starts or timeout limits
- **Enhanced Monitoring**: Better observability and debugging
- **Cost Efficiency**: No per-execution pricing

### Migration Checklist

- [x] Core analysis logic ported
- [x] YARA rules migrated  
- [x] AI analysis preserved
- [x] Event-driven architecture implemented
- [x] Database integration updated
- [x] Storage client migrated
- [x] Docker deployment configured
- [x] Health checks added
- [x] Logging improved
- [x] Tests updated

## Troubleshooting

### Common Issues

1. **NATS Connection Failed**
   - Check NATS server is running
   - Verify network connectivity
   - Confirm NATS_URL environment variable

2. **Database Connection Error**
   - Verify PostgreSQL is running
   - Check DATABASE_URL format
   - Confirm database permissions

3. **MinIO Access Denied**
   - Verify MinIO credentials
   - Check bucket exists
   - Confirm file permissions

4. **Analysis Timeout**
   - Check file size limits
   - Verify AI API key (if using)
   - Review YARA rule complexity

### Debug Mode

Enable verbose logging for debugging:

```bash
export LOG_LEVEL=DEBUG
python main.py
```

### Performance Tuning

- **Memory Usage**: Adjust file size limits
- **Analysis Speed**: Tune YARA rule complexity
- **AI Calls**: Configure request timeouts
- **Concurrency**: Scale worker instances

## Support

For issues and questions:

- **Documentation**: `/docs/0vers33r/`
- **Issue Tracker**: GitHub Issues
- **Discussions**: GitHub Discussions
- **Security**: Report to security@the0.dev