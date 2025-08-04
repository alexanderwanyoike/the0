import os
import yara
from typing import Tuple, List, Dict, Any, Optional, Set
from src.common.logging import logger


class RuleManager:
    """
    Manages YARA rules from separate files with multi-language support
    """

    def __init__(self, rules_directory: str = "yara_rules"):
        # Handle relative path from main.py location
        if not os.path.isabs(rules_directory):
            # Get the directory where main.py is located
            main_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
            self.rules_directory = os.path.join(main_dir, rules_directory)
        else:
            self.rules_directory = rules_directory

        self.compiled_rules = None
        self.rule_metadata = {}  # Store metadata for runtime filtering
        logger.info(f"🕵️ RuleManager: Using rules directory: {self.rules_directory}")

    def load_and_compile_rules(self) -> bool:
        """
        Load all .yar files from the rules directory and compile them with language awareness
        """
        try:
            rule_sources = {}
            self.rule_metadata = {}

            # Find all .yar files in the rules directory
            if os.path.exists(self.rules_directory):
                for filename in os.listdir(self.rules_directory):
                    if filename.endswith((".yar", ".yara")):
                        rule_path = os.path.join(self.rules_directory, filename)
                        
                        # Extract language/runtime info from filename
                        language = self._extract_language_from_filename(filename)
                        
                        try:
                            with open(rule_path, "r", encoding="utf-8") as f:
                                rule_content = f.read()
                                namespace = filename[:-4]  # Remove .yar extension
                                rule_sources[namespace] = rule_content
                                
                                # Store metadata for this rule file
                                self.rule_metadata[namespace] = {
                                    "language": language,
                                    "filename": filename,
                                    "path": rule_path
                                }
                            
                            logger.info(f"✅ Loaded rule file: {filename} (language: {language})")
                        except Exception as e:
                            logger.error(f"🚨 Failed to load YARA rule file {rule_path}: {str(e)}")
                            continue

            if not rule_sources:
                logger.warning("⚠️ No YARA rule files found, using fallback rules")
                rule_sources["fallback"] = self._get_fallback_rules()
                self.rule_metadata["fallback"] = {"language": "multi", "filename": "fallback", "path": "builtin"}

            # Compile all rules
            try:
                self.compiled_rules = yara.compile(sources=rule_sources)
            except yara.Error as e:
                # Extract filename from error message if possible
                error_msg = str(e)
                if ":" in error_msg:
                    problematic_file = error_msg.split(":")[0]
                    logger.error(f"🚨 Failed to compile YARA rules: Error in {problematic_file}: {error_msg}")
                else:
                    logger.error(f"🚨 Failed to compile YARA rules: {error_msg}")
                return False
            
            # Log rule statistics
            self._log_rule_statistics()
            return True

        except Exception as e:
            logger.error(f"🚨 Failed to compile YARA rules: {str(e)}")
            return False

    def _extract_language_from_filename(self, filename: str) -> str:
        """
        Extract language/runtime information from rule filename
        
        Expected patterns:
        - python_*.yar -> python
        - javascript_*.yar -> javascript
        - js_*.yar -> javascript
        - common_*.yar -> multi (applies to all)
        """
        filename_lower = filename.lower()
        
        if filename_lower.startswith("python"):
            return "python"
        elif filename_lower.startswith("javascript") or filename_lower.startswith("js_"):
            return "javascript"
        elif filename_lower.startswith("common") or filename_lower.startswith("universal"):
            return "multi"
        else:
            # Default to multi-language if pattern not recognized
            return "multi"

    def _log_rule_statistics(self):
        """Log statistics about loaded rules"""
        language_counts = {}
        for metadata in self.rule_metadata.values():
            lang = metadata["language"]
            language_counts[lang] = language_counts.get(lang, 0) + 1
        
        logger.info(f"📊 Rule Statistics: {dict(language_counts)}")
        logger.info(f"✅ Compiled {len(self.rule_metadata)} YARA rule files total")

    def _get_fallback_rules(self) -> str:
        """
        Fallback rules if no external files are found - covers basic patterns for both languages
        """
        return """
rule Critical_Code_Execution {
    meta:
        description = "Critical code execution functions (multi-language)"
        severity = "critical"
        language = "multi"
    strings:
        // Python patterns
        $py_eval = "eval(" nocase
        $py_exec = "exec(" nocase
        
        // JavaScript patterns
        $js_eval = "eval(" nocase
        $js_function = "new Function(" nocase
        $js_vm = "vm.runInNewContext(" nocase
    condition:
        any of them
}

rule System_Commands {
    meta:
        description = "System command execution (multi-language)"
        severity = "high"
        language = "multi"
    strings:
        // Python patterns
        $py_os_system = "os.system(" nocase
        $py_subprocess = "subprocess." nocase
        
        // JavaScript patterns
        $js_child_process = "child_process.exec(" nocase
        $js_spawn = "child_process.spawn(" nocase
    condition:
        any of them
}

rule Network_Requests {
    meta:
        description = "Suspicious network activity (multi-language)"
        severity = "medium"
        language = "multi"
    strings:
        // Python patterns
        $py_requests = "requests.post(" nocase
        $py_urllib = "urllib.request." nocase
        
        // JavaScript patterns
        $js_fetch = "fetch(" nocase
        $js_xhr = "XMLHttpRequest(" nocase
        
        // Suspicious domains/IPs
        $suspicious1 = "127.0.0.1" nocase
        $suspicious2 = ".onion" nocase
    condition:
        (any of ($py_*) or any of ($js_*)) and any of ($suspicious*)
}
"""

    def scan(
        self, data: bytes, filename: str = "unknown", runtime: Optional[str] = None
    ) -> Tuple[int, List[Dict[str, Any]]]:
        """
        Scan data with compiled rules, optionally filtering by runtime
        
        Args:
            data: File content to scan
            filename: Name of the file being scanned
            runtime: Runtime to filter rules for (e.g., "python3.11", "nodejs20")
        """
        if not self.compiled_rules:
            return 0, []

        try:
            matches = self.compiled_rules.match(data=data)
            return self._process_matches(matches, filename, runtime)
        except Exception as e:
            logger.error(f"🚨 YARA scan error: {str(e)}")
            return 2, [
                {"rule": "scan_error", "severity": "medium", "filename": filename}
            ]

    def get_applicable_rules(self, runtime: str) -> Set[str]:
        """
        Get list of rule namespaces applicable for a given runtime
        
        Args:
            runtime: Runtime environment (e.g., "python3.11", "nodejs20")
            
        Returns:
            Set of rule namespace names that apply to this runtime
        """
        applicable = set()
        
        # Map runtime to language
        language_map = {
            "python3.11": "python",
            "nodejs20": "javascript"
        }
        
        target_language = language_map.get(runtime, "multi")
        
        for namespace, metadata in self.rule_metadata.items():
            rule_language = metadata.get("language", "multi")
            
            # Include multi-language rules and rules for the specific language
            if rule_language == "multi" or rule_language == target_language:
                applicable.add(namespace)
        
        logger.debug(f"🔍 Applicable rules for {runtime}: {len(applicable)} rule files")
        return applicable

    def _process_matches(
        self, matches, filename: str, runtime: Optional[str] = None
    ) -> Tuple[int, List[Dict[str, Any]]]:
        """
        Process YARA matches and calculate threat score with optional runtime filtering
        
        Args:
            matches: YARA match objects
            filename: Name of the file being scanned
            runtime: Runtime to filter matches for
        """
        if not matches:
            return 0, []

        # Get applicable rule namespaces for this runtime
        applicable_namespaces = None
        if runtime:
            applicable_namespaces = self.get_applicable_rules(runtime)

        results = []
        max_score = 0
        filtered_count = 0

        for match in matches:
            # Filter by runtime if specified
            if applicable_namespaces and match.namespace not in applicable_namespaces:
                filtered_count += 1
                logger.debug(f"🚫 Filtered rule {match.rule} (namespace: {match.namespace}) for runtime {runtime}")
                continue

            # Extract metadata
            severity = "medium"  # default
            rule_language = "multi"  # default
            description = match.rule
            
            for k, v in match.meta.items():
                if k == "severity":
                    severity = v
                elif k == "language":
                    rule_language = v
                elif k == "description":
                    description = v

            # Convert severity to score
            severity_scores = {"low": 1, "medium": 2, "high": 3, "critical": 5}
            score = severity_scores.get(severity, 2)
            max_score = max(max_score, score)

            results.append(
                {
                    "rule": match.rule,
                    "namespace": match.namespace,
                    "severity": severity,
                    "score": score,
                    "filename": filename,
                    "language": rule_language,
                    "description": description,
                    "strings": [s.identifier for s in match.strings],
                    "runtime_filtered": runtime is not None
                }
            )

        if filtered_count > 0:
            logger.info(f"🔍 Filtered {filtered_count} rules not applicable to {runtime}")

        return max_score, results
