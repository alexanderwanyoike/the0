import os
import json
from typing import Dict, List, Any, Tuple, Optional
from dataclasses import dataclass
from google import genai
from src.common.logging import logger


@dataclass
class AIAnalysisResult:
    """Result of AI analysis"""
    score: int  # 0-5 scale
    reason: str
    confidence: float = 0.0
    error: Optional[str] = None


class AICodeAnalyzer:
    """
    AI-powered code analyzer using Gemini for security analysis
    """

    def __init__(self, api_key: Optional[str] = None, model_name: str = "gemini-2.5-flash-lite-preview-06-17"):
        """
        Initialize AI analyzer
        
        Args:
            api_key: Gemini API key (if None, will try to get from env)
            model_name: Gemini model to use
        """
        self.api_key = api_key or os.getenv("GEMINI_API_KEY")
        self.model_name = model_name
        self.client = None
        
        if not self.api_key:
            logger.warning("🤖 AI Analyzer: No API key provided, AI analysis will be disabled")
            return
            
        try:
            
            self.client = genai.Client(vertexai=True, project=os.getenv("GOOGLE_CLOUD_PROJECT"), location='global')
            logger.info(f"🤖 AI Analyzer: Initialized with model {self.model_name}")
        except Exception as e:
            logger.error(f"🤖 AI Analyzer: Failed to initialize: {str(e)}")
            self.client = None

    def is_available(self) -> bool:
        """Check if AI analysis is available"""
        return self.client is not None

    def analyze_code(self, code_files: Dict[str, str], runtime: str = "python3.11", bot_config: Optional[Dict[str, Any]] = None) -> AIAnalysisResult:
        """
        Analyze code files for security issues using AI
        
        Args:
            code_files: Dictionary of {filename: content}
            runtime: Runtime environment (python3.11 or nodejs20)
            bot_config: Bot configuration containing entrypoints and metadata
            
        Returns:
            AIAnalysisResult with score, reason, and confidence
        """
        if not self.is_available():
            return AIAnalysisResult(
                score=0, 
                reason="AI analysis unavailable",
                error="AI service not initialized"
            )

        try:
            # Prepare code for analysis
            code_context = self._prepare_code_context(code_files, runtime, bot_config)
            
            # Generate analysis prompt
            prompt = self._generate_analysis_prompt(code_context, runtime)
            
            # Call Gemini API
            logger.info(f"🤖 AI Analyzer: Analyzing {len(code_files)} files for {runtime}")
            response = self.client.models.generate_content(
                model=self.model_name,
                contents=prompt,
            )
            
            # Parse response
            return self._parse_ai_response(response.text)
            
        except Exception as e:
            logger.error(f"🤖 AI Analyzer: Analysis failed: {str(e)}")
            return AIAnalysisResult(
                score=2,  # Medium score on error to be safe
                reason=f"AI analysis failed: {str(e)}",
                error=str(e)
            )

    def _prepare_code_context(self, code_files: Dict[str, str], runtime: str, bot_config: Optional[Dict[str, Any]] = None) -> str:
        """
        Prepare code context for AI analysis with entrypoint prioritization
        """
        # Utilize Gemini 2.5 Flash's 1M token capacity - target ~800k characters (~200k tokens)
        # Leave room for prompt overhead and response tokens
        max_total_chars = 800000
        total_chars = 0
        
        context_parts = []
        context_parts.append(f"Runtime: {runtime}\n")
        context_parts.append(f"Total files: {len(code_files)}\n")
        
        # Extract entrypoints from bot config if available
        entrypoint_files = []
        if bot_config and 'entrypoints' in bot_config:
            entrypoints = bot_config['entrypoints']
            bot_entrypoint = entrypoints.get('bot')
            backtest_entrypoint = entrypoints.get('backtest')
            
            if bot_entrypoint:
                entrypoint_files.append(bot_entrypoint)
            if backtest_entrypoint:
                entrypoint_files.append(backtest_entrypoint)
            
            if entrypoint_files:
                context_parts.append(f"Bot entrypoints: {', '.join(entrypoint_files)}\n")
        
        context_parts.append("\n")
        
        # Categorize files by priority
        priority_files = []
        regular_files = []
        
        for filename, content in code_files.items():
            # Highest priority: Bot config entrypoints
            if filename in entrypoint_files:
                priority_files.insert(0, (filename, content))  # Insert at beginning
            # Medium priority: Common entry points and config files  
            elif any(pattern in filename.lower() for pattern in ['main.', 'index.', 'app.', 'config', 'setup']):
                priority_files.append((filename, content))
            else:
                regular_files.append((filename, content))
        
        # Add files in priority order - no individual file size limits
        files_added = 0
        for filename, content in priority_files + regular_files:
            file_header = f"\n--- FILE: {filename} ---\n"
            proposed_addition = file_header + content
            
            if total_chars + len(proposed_addition) > max_total_chars:
                remaining_files = len(priority_files) + len(regular_files) - files_added
                context_parts.append(f"\n... [Additional {remaining_files} files truncated - reached context limit] ...")
                break
                
            context_parts.append(proposed_addition)
            total_chars += len(proposed_addition)
            files_added += 1
        
        logger.info(f"🤖 AI Context: {files_added}/{len(code_files)} files, {total_chars:,} characters")
        return "".join(context_parts)

    def _generate_analysis_prompt(self, code_context: str, runtime: str) -> str:
        """
        Generate analysis prompt for AI
        """
        system_prompt = os.getenv("AI_SYSTEM_PROMPT", self._get_default_system_prompt())
        logger.info(f"🤖 AI Analyzer: Using system prompt: {system_prompt[:100]}...")  # Log first 100 chars
        
        prompt = f"""{system_prompt}

Please analyze the following {runtime} code for security vulnerabilities and malicious patterns.

{code_context}

Provide your analysis as a JSON object with exactly this structure:
{{
    "score": <integer from 0-5>,
    "reason": "<detailed explanation>",
    "confidence": <float from 0.0-1.0>
}}

Score meanings:
- 0-1: Safe code, no security concerns
- 2-3: Potentially unsafe, requires human review
- 4-5: Unsafe code, likely malicious

Focus on:
- Code injection vulnerabilities
- Suspicious network activities
- Credential theft attempts
- System compromise patterns
- Cryptocurrency/trading bot abuse
- Data exfiltration

Respond ONLY with the JSON object, no additional text."""

        return prompt

    def _get_default_system_prompt(self) -> str:
        """
        Get default system prompt for code analysis
        """
        return """You are a cybersecurity expert analyzing code for malicious patterns and security vulnerabilities. 
You specialize in detecting:
- Code injection attacks
- Reverse shells and backdoors
- Credential harvesting
- Data exfiltration
- Cryptocurrency theft
- Trading bot manipulation
- System compromise attempts

You must be thorough but balanced - legitimate trading bots and automation tools should not be flagged as malicious unless they contain actual security threats."""

    def _parse_ai_response(self, response_text: str) -> AIAnalysisResult:
        """
        Parse AI response into structured result
        """
        try:
            # Try to extract JSON from response
            response_text = response_text.strip()
            
            # Handle case where response might have extra text
            start_idx = response_text.find('{')
            end_idx = response_text.rfind('}') + 1
            
            if start_idx == -1 or end_idx == 0:
                raise ValueError("No JSON found in response")
            
            json_text = response_text[start_idx:end_idx]
            result = json.loads(json_text)
            
            # Validate required fields
            score = int(result.get("score", 0))
            reason = str(result.get("reason", "No reason provided"))
            confidence = float(result.get("confidence", 0.0))
            
            # Validate score range
            if not (0 <= score <= 5):
                logger.warning(f"🤖 AI Analyzer: Invalid score {score}, clamping to 0-5")
                score = max(0, min(5, score))
            
            # Validate confidence range
            if not (0.0 <= confidence <= 1.0):
                logger.warning(f"🤖 AI Analyzer: Invalid confidence {confidence}, clamping to 0.0-1.0")
                confidence = max(0.0, min(1.0, confidence))
            
            return AIAnalysisResult(
                score=score,
                reason=reason,
                confidence=confidence
            )
            
        except Exception as e:
            logger.error(f"🤖 AI Analyzer: Failed to parse response: {str(e)}")
            logger.debug(f"🤖 AI Response text: {response_text[:500]}...")
            
            # Fallback: try to extract a score from the text
            fallback_score = self._extract_fallback_score(response_text)
            
            return AIAnalysisResult(
                score=fallback_score,
                reason=f"Failed to parse AI response, using fallback analysis. Original error: {str(e)}",
                confidence=0.0,
                error=f"Parse error: {str(e)}"
            )

    def _extract_fallback_score(self, response_text: str) -> int:
        """
        Extract a fallback score from unstructured response
        """
        response_lower = response_text.lower()
        
        # Look for danger indicators
        danger_keywords = ['malicious', 'dangerous', 'exploit', 'attack', 'vulnerable', 'unsafe']
        safe_keywords = ['safe', 'benign', 'legitimate', 'normal', 'secure']
        
        danger_count = sum(1 for keyword in danger_keywords if keyword in response_lower)
        safe_count = sum(1 for keyword in safe_keywords if keyword in response_lower)
        
        if danger_count > safe_count:
            return 4  # High risk
        elif safe_count > danger_count:
            return 1  # Low risk
        else:
            return 2  # Medium risk (uncertain)