import io
import zipfile
from typing import Dict, Any, Tuple, List

from src.common.logging import logger
from src.interfaces import StorageInterface, DatabaseInterface
from src.rule_manager import RuleManager
from src.ai_analyzer import AICodeAnalyzer, AIAnalysisResult


class The0vers33r:
    """
    0vers33r Analyzer with dependency injection for easy testing
    """

    def __init__(
        self,
        rules_directory: str = "yara_rules",
        storage_client: StorageInterface = None,
        database_client: DatabaseInterface = None,
        enable_ai_analysis: bool = True,
    ):
        self.rule_manager = RuleManager(rules_directory)
        self.storage_client = storage_client
        self.database_client = database_client
        
        # Initialize AI analyzer
        self.ai_analyzer = None
        if enable_ai_analysis:
            self.ai_analyzer = AICodeAnalyzer()
            if self.ai_analyzer.is_available():
                logger.info("🤖 AI analysis enabled")
            else:
                logger.warning("🤖 AI analysis disabled (no API key or initialization failed)")

        # Initialize rules
        if not self.rule_manager.load_and_compile_rules():
            raise Exception("Failed to initialize YARA rules")

    def analyze_bot(self, bot_data: Dict[str, Any]) -> Tuple[str, Dict[str, Any]]:
        """
        Main analysis pipeline with runtime detection
        """
        review_data = {
            "reviewedAt": "SERVER_TIMESTAMP",  # Will be replaced by concrete implementation
            "reviewedBy": "0vers33r",
            "version": "v2.1-js-ai-support",
            "score": 0,
            "issues": [],
            "filesScanned": [],
            "yaraMatches": [],
            "totalFiles": 0,
            "threatSummary": {},
            "runtime": "unknown",
            "aiAnalysis": {
                "enabled": self.ai_analyzer is not None and self.ai_analyzer.is_available(),
                "score": 0,
                "reason": "",
                "confidence": 0.0,
                "error": None
            },
        }

        try:
            logger.info(f"🕵️ 0VERS33R: Analyzing bot {bot_data.get('id', 'unknown')}")

            # Basic validation
            if not self._basic_validation(bot_data):
                review_data["score"] = 5
                review_data["issues"] = ["invalid_bot_structure"]
                return "declined", review_data

            # Detect runtime from bot configuration
            runtime = self._detect_runtime(bot_data)
            review_data["runtime"] = runtime
            logger.info(f"🔍 0VERS33R: Detected runtime: {runtime}")

            # Scan code from storage
            file_path = bot_data.get("filePath", "")
            code_files = {}  # Store code files for AI analysis
            
            logger.info(f"🔍 File path from bot data: {file_path}")
            
            if file_path and self.storage_client:
                scan_results, code_files = self._scan_gcs_code(file_path, runtime)
                review_data.update(scan_results)

            # Generate threat summary from YARA results
            review_data["threatSummary"] = self._generate_threat_summary(
                review_data["yaraMatches"]
            )

            # Determine preliminary status based on YARA results
            yara_score = review_data["score"]
            
            # If YARA finds critical issues (score >= 4), reject immediately
            if yara_score >= 4:
                logger.info(f"🚨 0VERS33R: YARA found critical issues (score: {yara_score}), skipping AI analysis")
                return "declined", review_data
            
            # If YARA passes, run AI analysis for final decision
            if self.ai_analyzer and self.ai_analyzer.is_available() and code_files:
                logger.info("🤖 0VERS33R: Running AI analysis...")
                bot_config = bot_data.get("config", {})
                ai_result = self.ai_analyzer.analyze_code(code_files, runtime, bot_config)
                
                # Update review data with AI results
                review_data["aiAnalysis"] = {
                    "enabled": True,
                    "score": ai_result.score,
                    "reason": ai_result.reason,
                    "confidence": ai_result.confidence,
                    "error": ai_result.error
                }
                
                # Combine YARA and AI scores (take the maximum)
                combined_score = max(yara_score, ai_result.score)
                review_data["score"] = combined_score
                
                # Add AI issues to the issues list
                if ai_result.score > 0:
                    review_data["issues"].append(f"ai_analysis:score_{ai_result.score}")
                
                logger.info(f"🤖 0VERS33R: AI analysis complete (score: {ai_result.score}, combined: {combined_score})")
            else:
                logger.info("🤖 0VERS33R: AI analysis skipped (disabled or no code files)")

            # Determine final status based on combined score
            final_score = review_data["score"]
            if final_score == 0:
                return "approved", review_data
            elif final_score <= 2:
                return "approved", review_data
            elif final_score <= 3:
                return "awaiting_human_review", review_data
            else:
                return "declined", review_data

        except Exception as e:
            logger.error(f"🚨 0VERS33R: Analysis failed: {str(e)}")
            review_data["score"] = 5
            review_data["issues"] = [f"analysis_error_{type(e).__name__}"]
            return "declined", review_data

    def analyze_and_update_bot(
        self, bot_id: str, bot_data: Dict[str, Any]
    ) -> Tuple[str, Dict[str, Any]]:
        """
        Analyze bot and update database if database client is available
        """
        status, review_data = self.analyze_bot(bot_data)

        if self.database_client:
            self.database_client.update_bot_status(bot_id, status, review_data)

        return status, review_data

    def _basic_validation(self, bot_data: Dict[str, Any]) -> bool:
        """Basic validation"""
        required = ["name", "config", "filePath", "userId"]
        missing_fields = [field for field in required if field not in bot_data or not bot_data[field]]
        if missing_fields:
            logger.warning(f"🔍 Missing required fields: {missing_fields}")
            logger.warning(f"🔍 Available fields: {list(bot_data.keys())}")
        return all(field in bot_data and bot_data[field] for field in required)

    def _detect_runtime(self, bot_data: Dict[str, Any]) -> str:
        """
        Detect runtime from bot configuration
        """
        try:
            config = bot_data.get("config", {})
            runtime = config.get("runtime", "unknown")
            
            # Validate runtime values
            valid_runtimes = ["python3.11", "nodejs20"]
            if runtime in valid_runtimes:
                return runtime
            
            # Fallback: try to detect from file extensions or other hints
            logger.warning(f"🔍 Unknown runtime '{runtime}', attempting auto-detection")
            return "python3.11"  # Default fallback
            
        except Exception as e:
            logger.warning(f"⚠️ Runtime detection failed: {str(e)}, defaulting to python3.11")
            return "python3.11"

    def _scan_gcs_code(self, gcs_path: str, runtime: str = "python3.11") -> Tuple[Dict[str, Any], Dict[str, str]]:
        """
        Download and scan code from storage with runtime-aware filtering
        Returns: (scan_results, code_files_for_ai)
        """
        results = {
            "score": 0,
            "filesScanned": [],
            "yaraMatches": [],
            "issues": [],
            "totalFiles": 0,
        }

        try:
            if not self.storage_client:
                results["issues"] = ["no_storage_client"]
                results["score"] = 5
                return results, {}

            # Download file using injected storage client
            zip_content = self.storage_client.download_file(gcs_path)
            return self._scan_zip_contents(zip_content, results, runtime)

        except Exception as e:
            logger.error(f"🚨 Storage scan error: {str(e)}")
            results["score"] = 5
            results["issues"] = [f"storage_error_{type(e).__name__}"]
            return results, {}

    def _scan_zip_contents(
        self, zip_content: bytes, results: Dict[str, Any], runtime: str = "python3.11"
    ) -> Tuple[Dict[str, Any], Dict[str, str]]:
        """
        Extract and scan zip contents with runtime-aware filtering
        Returns: (scan_results, code_files_for_ai)
        """
        try:
            with zipfile.ZipFile(io.BytesIO(zip_content), "r") as zip_file:
                file_list = zip_file.namelist()
                results["totalFiles"] = len(file_list)

                # Scan each relevant file
                max_score = 0
                code_files = {}  # Collect code files for AI analysis
                
                for file_path in file_list:
                    # Skip third-party dependency directories based on runtime
                    if self._should_skip_file(file_path, runtime):
                        logger.info(f"Skipping dependency file: {file_path}")
                        continue
                        
                    if self._should_scan_file(file_path, runtime):
                        try:
                            file_content = zip_file.read(file_path)
                            
                            # Store code files for AI analysis (decode text files)
                            try:
                                # Try to decode as text for AI analysis
                                text_content = file_content.decode('utf-8')
                                code_files[file_path] = text_content
                            except UnicodeDecodeError:
                                # Skip binary files for AI analysis but still scan with YARA
                                logger.debug(f"Skipping binary file for AI analysis: {file_path}")
                            
                            # YARA scan with runtime filtering
                            file_score, file_matches = self.rule_manager.scan(
                                file_content, file_path, runtime
                            )

                            results["filesScanned"].append(file_path)

                            if file_matches:
                                results["yaraMatches"].extend(file_matches)
                                for match in file_matches:
                                    results["issues"].append(
                                        f"{file_path}:{match['rule']}"
                                    )

                            max_score = max(max_score, file_score)

                            # Early exit on critical findings
                            if file_score >= 5:
                                break

                        except Exception as e:
                            logger.warning(f"⚠️ Failed to scan {file_path}: {str(e)}")
                            results["issues"].append(f"scan_error_{file_path}")

                results["score"] = max_score
                return results, code_files

        except zipfile.BadZipFile:
            results["score"] = 5
            results["issues"] = ["corrupted_zip"]
            return results, {}
        except Exception as e:
            results["score"] = 5
            results["issues"] = [f"zip_error_{type(e).__name__}"]
            return results, {}

    def _should_skip_file(self, file_path: str, runtime: str) -> bool:
        """
        Determine if file should be skipped based on runtime and dependency directories
        """
        # Common directories to skip for all runtimes
        skip_dirs = ["__pycache__/", ".git/", ".vscode/", ".idea/", "dist/", "build/"]
        
        # Runtime-specific dependency directories
        if runtime == "python3.11":
            skip_dirs.extend(["vendor/", "venv/", ".venv/", "site-packages/"])
        elif runtime == "nodejs20":
            skip_dirs.extend(["node_modules/", ".npm/", "dist/", "build/"])
        
        # Check if file is in any skip directory
        for skip_dir in skip_dirs:
            if skip_dir in file_path:
                return True
        
        return False

    def _should_scan_file(self, filename: str, runtime: str = "unknown") -> bool:
        """Determine if file should be scanned based on runtime"""
        
        # Common extensions for all runtimes
        common_extensions = {".json", ".yaml", ".yml", ".sh", ".bat", ".txt", ".md"}
        
        # Runtime-specific extensions
        if runtime == "python3.11":
            python_extensions = {".py", ".pyx", ".pyi"}
            scan_extensions = common_extensions.union(python_extensions)
        elif runtime == "nodejs20":
            js_extensions = {".js", ".ts", ".jsx", ".tsx", ".mjs", ".cjs"}
            scan_extensions = common_extensions.union(js_extensions)
        else:
            # Default to scanning all supported extensions
            scan_extensions = {
                ".py", ".js", ".ts", ".jsx", ".tsx", ".mjs", ".cjs",
                ".json", ".yaml", ".yml", ".sh", ".bat", ".txt", ".md"
            }
        
        return any(filename.lower().endswith(ext) for ext in scan_extensions)

    def _generate_threat_summary(
        self, yara_matches: List[Dict[str, Any]]
    ) -> Dict[str, Any]:
        """
        Generate a summary of detected threats
        """
        if not yara_matches:
            return {
                "threatLevel": "none",
                "categories": {},
                "criticalRules": [],
                "totalMatches": 0,
            }

        # Count by severity
        severity_counts = {"low": 0, "medium": 0, "high": 0, "critical": 0}
        rule_categories = {}
        critical_rules = []

        for match in yara_matches:
            severity = match.get("severity", "medium")
            if severity in severity_counts:
                severity_counts[severity] += 1

            # Track rule categories
            rule_name = match["rule"]
            category = rule_name.split("_")[0] if "_" in rule_name else "other"
            rule_categories[category] = rule_categories.get(category, 0) + 1

            # Track critical rules
            if severity == "critical":
                critical_rules.append(rule_name)

        # Determine overall threat level
        if severity_counts["critical"] > 0:
            threat_level = "critical"
        elif severity_counts["high"] > 0:
            threat_level = "high"
        elif severity_counts["medium"] > 2:
            threat_level = "medium"
        else:
            threat_level = "low"

        return {
            "threatLevel": threat_level,
            "severityCounts": severity_counts,
            "ruleCategories": rule_categories,
            "criticalRules": critical_rules,
            "totalMatches": len(yara_matches),
        }
