import unittest
import os
import tempfile
import zipfile
from unittest.mock import Mock, patch, MagicMock

from src.analyzer import The0vers33r
from src.ai_analyzer import AICodeAnalyzer, AIAnalysisResult
from src.tests.mocks import MockStorageClient, MockDatabaseClient


class TestAIAnalysis(unittest.TestCase):
    """
    Test suite for AI analysis integration in 0VERS33R
    """

    def setUp(self):
        """Set up test environment"""
        self.temp_dir = tempfile.mkdtemp()
        self.mock_storage = MockStorageClient()
        self.mock_database = MockDatabaseClient()

    def tearDown(self):
        """Clean up test environment"""
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    @patch.dict(os.environ, {'GEMINI_API_KEY': 'test_api_key'})
    def test_ai_analyzer_initialization(self):
        """Test AI analyzer initialization with API key"""
        with patch('src.ai_analyzer.genai') as mock_genai:
            mock_model = Mock()
            mock_genai.GenerativeModel.return_value = mock_model
            
            ai_analyzer = AICodeAnalyzer()
            
            self.assertTrue(ai_analyzer.is_available())
            mock_genai.configure.assert_called_once_with(api_key='test_api_key')

    def test_ai_analyzer_no_api_key(self):
        """Test AI analyzer when no API key is provided"""
        with patch.dict(os.environ, {}, clear=True):
            ai_analyzer = AICodeAnalyzer()
            self.assertFalse(ai_analyzer.is_available())

    @patch.dict(os.environ, {'GEMINI_API_KEY': 'test_api_key'})
    def test_ai_analyzer_code_analysis(self):
        """Test AI code analysis functionality"""
        with patch('src.ai_analyzer.genai') as mock_genai:
            # Mock Gemini response
            mock_response = Mock()
            mock_response.text = '''
            {
                "score": 4,
                "reason": "Code contains eval() function which is dangerous for code injection",
                "confidence": 0.9
            }
            '''
            
            mock_model = Mock()
            mock_model.generate_content.return_value = mock_response
            mock_genai.GenerativeModel.return_value = mock_model
            
            ai_analyzer = AICodeAnalyzer()
            
            # Test malicious code
            code_files = {
                "malicious.js": "const result = eval(userInput);"
            }
            
            # Test with bot config
            bot_config = {
                "entrypoints": {
                    "bot": "malicious.js",
                    "backtest": "backtest.js"
                }
            }
            
            result = ai_analyzer.analyze_code(code_files, "nodejs20", bot_config)
            
            self.assertEqual(result.score, 4)
            self.assertIn("eval()", result.reason)
            self.assertEqual(result.confidence, 0.9)
            self.assertIsNone(result.error)

    @patch.dict(os.environ, {'GEMINI_API_KEY': 'test_api_key'})
    def test_ai_analyzer_parse_error_handling(self):
        """Test AI analyzer error handling for malformed responses"""
        with patch('src.ai_analyzer.genai') as mock_genai:
            # Mock malformed response
            mock_response = Mock()
            mock_response.text = "This is not JSON format"
            
            mock_model = Mock()
            mock_model.generate_content.return_value = mock_response
            mock_genai.GenerativeModel.return_value = mock_model
            
            ai_analyzer = AICodeAnalyzer()
            
            code_files = {"test.js": "console.log('test');"}
            result = ai_analyzer.analyze_code(code_files, "nodejs20")
            
            # Should handle parse error gracefully
            self.assertIsNotNone(result.error)
            self.assertGreater(result.score, 0)  # Fallback score

    @patch.dict(os.environ, {'GEMINI_API_KEY': 'test_api_key'})
    def test_ai_analyzer_api_exception(self):
        """Test AI analyzer handling of API exceptions"""
        with patch('src.ai_analyzer.genai') as mock_genai:
            mock_model = Mock()
            mock_model.generate_content.side_effect = Exception("API Error")
            mock_genai.GenerativeModel.return_value = mock_model
            
            ai_analyzer = AICodeAnalyzer()
            
            code_files = {"test.js": "console.log('test');"}
            result = ai_analyzer.analyze_code(code_files, "nodejs20")
            
            # Should handle API error gracefully
            self.assertIsNotNone(result.error)
            self.assertEqual(result.score, 2)  # Medium risk on error

    def test_analyzer_with_ai_disabled(self):
        """Test analyzer behavior when AI is disabled"""
        analyzer = The0vers33r(
            rules_directory="yara_rules",
            storage_client=self.mock_storage,
            database_client=self.mock_database,
            enable_ai_analysis=False
        )
        
        # Should not have AI analyzer
        self.assertIsNone(analyzer.ai_analyzer)

    @patch.dict(os.environ, {'GEMINI_API_KEY': 'test_api_key'})
    def test_analyzer_integration_with_ai(self):
        """Test full analyzer integration with AI analysis"""
        with patch('src.ai_analyzer.genai') as mock_genai:
            # Mock AI response
            mock_response = Mock()
            mock_response.text = '''
            {
                "score": 3,
                "reason": "Code contains suspicious network requests",
                "confidence": 0.8
            }
            '''
            
            mock_model = Mock()
            mock_model.generate_content.return_value = mock_response
            mock_genai.GenerativeModel.return_value = mock_model
            
            analyzer = The0vers33r(
                rules_directory="yara_rules",
                storage_client=self.mock_storage,
                database_client=self.mock_database,
                enable_ai_analysis=True
            )
            
            # Create test zip with code that passes YARA but triggers AI
            zip_content = self._create_test_zip({
                "suspicious.js": """
                fetch('https://suspicious-domain.com/data')
                    .then(response => response.json())
                    .then(data => console.log(data));
                """
            })
            
            self.mock_storage.zip_content = zip_content
            
            bot_data = {
                "name": "suspicious-bot",
                "config": {"runtime": "nodejs20"},
                "gcsPath": "suspicious.zip",
                "userId": "test-user"
            }
            
            status, review_data = analyzer.analyze_bot(bot_data)
            
            # Should include AI analysis results
            self.assertTrue(review_data["aiAnalysis"]["enabled"])
            self.assertEqual(review_data["aiAnalysis"]["score"], 3)
            self.assertIn("suspicious", review_data["aiAnalysis"]["reason"])
            self.assertEqual(review_data["aiAnalysis"]["confidence"], 0.8)

    @patch.dict(os.environ, {'GEMINI_API_KEY': 'test_api_key'})
    def test_analyzer_yara_critical_skips_ai(self):
        """Test that critical YARA findings skip AI analysis"""
        with patch('src.ai_analyzer.genai') as mock_genai:
            mock_model = Mock()
            mock_genai.GenerativeModel.return_value = mock_model
            
            analyzer = The0vers33r(
                rules_directory="yara_rules",
                storage_client=self.mock_storage,
                database_client=self.mock_database,
                enable_ai_analysis=True
            )
            
            # Mock YARA to return critical score
            with patch.object(analyzer.rule_manager, 'scan') as mock_scan:
                mock_scan.return_value = (5, [{"rule": "critical_threat", "severity": "critical"}])
                
                zip_content = self._create_test_zip({
                    "malicious.js": "eval(userInput);"
                })
                
                self.mock_storage.zip_content = zip_content
                
                bot_data = {
                    "name": "critical-bot",
                    "config": {"runtime": "nodejs20"},
                    "gcsPath": "critical.zip",
                    "userId": "test-user"
                }
                
                status, review_data = analyzer.analyze_bot(bot_data)
                
                # Should decline due to YARA, AI should not be called
                self.assertEqual(status, "declined")
                self.assertEqual(review_data["score"], 5)
                # AI should not have been called (original values)
                self.assertEqual(review_data["aiAnalysis"]["score"], 0)

    @patch.dict(os.environ, {'GEMINI_API_KEY': 'test_api_key'})
    def test_combined_yara_ai_scoring(self):
        """Test combined scoring from YARA and AI analysis"""
        with patch('src.ai_analyzer.genai') as mock_genai:
            # Mock AI to return higher score than YARA
            mock_response = Mock()
            mock_response.text = '''
            {
                "score": 4,
                "reason": "High-risk patterns detected",
                "confidence": 0.95
            }
            '''
            
            mock_model = Mock()
            mock_model.generate_content.return_value = mock_response
            mock_genai.GenerativeModel.return_value = mock_model
            
            analyzer = The0vers33r(
                rules_directory="yara_rules",
                storage_client=self.mock_storage,
                database_client=self.mock_database,
                enable_ai_analysis=True
            )
            
            # Mock YARA to return low score
            with patch.object(analyzer.rule_manager, 'scan') as mock_scan:
                mock_scan.return_value = (1, [{"rule": "low_risk", "severity": "low"}])
                
                zip_content = self._create_test_zip({
                    "test.js": "console.log('test');"
                })
                
                self.mock_storage.zip_content = zip_content
                
                bot_data = {
                    "name": "test-bot",
                    "config": {"runtime": "nodejs20"},
                    "gcsPath": "test.zip",
                    "userId": "test-user"
                }
                
                status, review_data = analyzer.analyze_bot(bot_data)
                
                # Should take maximum of YARA (1) and AI (4) scores
                self.assertEqual(review_data["score"], 4)
                self.assertEqual(status, "declined")  # Score 4 should decline

    def test_ai_code_preparation(self):
        """Test AI code preparation and context building"""
        with patch.dict(os.environ, {'GEMINI_API_KEY': 'test_api_key'}):
            with patch('src.ai_analyzer.genai') as mock_genai:
                mock_model = Mock()
                mock_genai.GenerativeModel.return_value = mock_model
                
                ai_analyzer = AICodeAnalyzer()
                
                code_files = {
                    "main.js": "console.log('main');",
                    "helper.js": "function help() { return 'help'; }",
                    "config.json": '{"name": "test"}'
                }
                
                context = ai_analyzer._prepare_code_context(code_files, "nodejs20")
                
                # Should include runtime info
                self.assertIn("Runtime: nodejs20", context)
                self.assertIn("Total files: 3", context)
                
                # Should include file contents
                self.assertIn("main.js", context)
                self.assertIn("console.log", context)

    def _create_test_zip(self, files_dict):
        """Helper to create a test zip file"""
        temp_zip = tempfile.NamedTemporaryFile(delete=False, suffix='.zip')
        
        with zipfile.ZipFile(temp_zip, 'w') as zf:
            for filename, content in files_dict.items():
                zf.writestr(filename, content)
        
        temp_zip.close()
        
        with open(temp_zip.name, 'rb') as f:
            zip_content = f.read()
        
        os.unlink(temp_zip.name)
        return zip_content


if __name__ == '__main__':
    unittest.main()