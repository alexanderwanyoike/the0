import unittest
import os
import tempfile
import zipfile
from unittest.mock import Mock, patch

from src.analyzer import The0vers33r
from src.tests.mocks import MockStorageClient, MockDatabaseClient


class TestJavaScriptSupport(unittest.TestCase):
    """
    Test suite for JavaScript support in 0VERS33R analyzer
    """

    def setUp(self):
        """Set up test environment"""
        self.temp_dir = tempfile.mkdtemp()
        self.mock_storage = MockStorageClient()
        self.mock_database = MockDatabaseClient()

        # Create analyzer with JS support (disable AI for these tests)
        self.analyzer = The0vers33r(
            rules_directory="yara_rules",
            storage_client=self.mock_storage,
            database_client=self.mock_database,
            enable_ai_analysis=False
        )

    def tearDown(self):
        """Clean up test environment"""
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_runtime_detection_nodejs(self):
        """Test detection of nodejs runtime"""
        bot_data = {
            "name": "test-bot",
            "config": {"runtime": "nodejs20"},
            "filePath": "test.zip",
            "userId": "test-user"
        }
        
        runtime = self.analyzer._detect_runtime(bot_data)
        self.assertEqual(runtime, "nodejs20")

    def test_runtime_detection_python(self):
        """Test detection of python runtime"""
        bot_data = {
            "name": "test-bot", 
            "config": {"runtime": "python3.11"},
            "filePath": "test.zip",
            "userId": "test-user"
        }
        
        runtime = self.analyzer._detect_runtime(bot_data)
        self.assertEqual(runtime, "python3.11")

    def test_runtime_detection_fallback(self):
        """Test fallback when runtime is unknown"""
        bot_data = {
            "name": "test-bot",
            "config": {"runtime": "unknown"},
            "filePath": "test.zip", 
            "userId": "test-user"
        }
        
        runtime = self.analyzer._detect_runtime(bot_data)
        self.assertEqual(runtime, "python3.11")  # Should fallback to python

    def test_should_scan_javascript_files(self):
        """Test that JavaScript files are scanned for nodejs runtime"""
        # Test JS files
        self.assertTrue(self.analyzer._should_scan_file("main.js", "nodejs20"))
        self.assertTrue(self.analyzer._should_scan_file("index.ts", "nodejs20"))
        self.assertTrue(self.analyzer._should_scan_file("app.jsx", "nodejs20"))
        self.assertTrue(self.analyzer._should_scan_file("component.tsx", "nodejs20"))
        self.assertTrue(self.analyzer._should_scan_file("module.mjs", "nodejs20"))
        
        # Test common files
        self.assertTrue(self.analyzer._should_scan_file("package.json", "nodejs20"))
        self.assertTrue(self.analyzer._should_scan_file("config.yaml", "nodejs20"))

    def test_should_scan_python_files(self):
        """Test that Python files are scanned for python runtime"""
        # Test Python files
        self.assertTrue(self.analyzer._should_scan_file("main.py", "python3.11"))
        self.assertTrue(self.analyzer._should_scan_file("module.pyx", "python3.11"))
        self.assertTrue(self.analyzer._should_scan_file("types.pyi", "python3.11"))
        
        # Should not scan JS files for Python runtime
        self.assertFalse(self.analyzer._should_scan_file("main.js", "python3.11"))
        self.assertFalse(self.analyzer._should_scan_file("index.ts", "python3.11"))

    def test_should_skip_node_modules(self):
        """Test that node_modules directories are skipped for nodejs runtime"""
        # Should skip node_modules
        self.assertTrue(self.analyzer._should_skip_file("node_modules/react/index.js", "nodejs20"))
        self.assertTrue(self.analyzer._should_skip_file("src/node_modules/lib/file.js", "nodejs20"))
        
        # Should not skip vendor for nodejs
        self.assertFalse(self.analyzer._should_skip_file("vendor/lib.js", "nodejs20"))

    def test_should_skip_vendor_python(self):
        """Test that vendor directories are skipped for python runtime"""
        # Should skip vendor
        self.assertTrue(self.analyzer._should_skip_file("vendor/django/module.py", "python3.11"))
        self.assertTrue(self.analyzer._should_skip_file("src/vendor/lib/file.py", "python3.11"))
        
        # Should not skip node_modules for python
        self.assertFalse(self.analyzer._should_skip_file("node_modules/lib.py", "python3.11"))

    def test_javascript_malicious_code_detection(self):
        """Test detection of malicious JavaScript patterns"""
        # Create a zip with malicious JS code
        zip_content = self._create_test_zip({
            "malicious.js": """
            const userInput = process.argv[2];
            const result = eval(userInput);
            const vm = require('vm');
            vm.runInNewContext(userInput);
            """,
            "package.json": '{"name": "test", "main": "malicious.js"}'
        })
        
        self.mock_storage.add_file("malicious.zip", zip_content)
        
        bot_data = {
            "name": "malicious-bot",
            "config": {"runtime": "nodejs20"},
            "filePath": "malicious.zip",
            "userId": "test-user"
        }
        
        status, review_data = self.analyzer.analyze_bot(bot_data)
        
        # Should detect malicious patterns
        self.assertGreater(review_data["score"], 0)
        self.assertGreater(len(review_data["yaraMatches"]), 0)
        self.assertEqual(review_data["runtime"], "nodejs20")

    def test_javascript_benign_code_detection(self):
        """Test that benign JavaScript code doesn't trigger false positives"""
        # Create a zip with benign JS code
        zip_content = self._create_test_zip({
            "index.js": """
            const express = require('express');
            const app = express();
            
            app.get('/', (req, res) => {
                res.send('Hello World');
            });
            
            app.listen(3000, () => {
                console.log('Server running on port 3000');
            });
            """,
            "package.json": '{"name": "web-app", "main": "index.js"}'
        })
        
        self.mock_storage.add_file("benign.zip", zip_content)
        
        bot_data = {
            "name": "web-app",
            "config": {"runtime": "nodejs20"},
            "filePath": "benign.zip",
            "userId": "test-user"
        }
        
        status, review_data = self.analyzer.analyze_bot(bot_data)
        
        # Should not detect issues in benign code
        self.assertEqual(review_data["score"], 0)
        self.assertEqual(len(review_data["yaraMatches"]), 0)
        self.assertEqual(status, "approved")

    def test_mixed_runtime_files(self):
        """Test handling of mixed Python and JavaScript files"""
        # Create a zip with both Python and JS files
        zip_content = self._create_test_zip({
            "main.py": "print('Hello from Python')",
            "helper.js": "console.log('Hello from JavaScript');",
            "config.json": '{"runtime": "nodejs20"}'
        })
        
        self.mock_storage.add_file("mixed.zip", zip_content)
        
        bot_data = {
            "name": "mixed-bot",
            "config": {"runtime": "nodejs20"},
            "filePath": "mixed.zip",
            "userId": "test-user"
        }
        
        status, review_data = self.analyzer.analyze_bot(bot_data)
        
        # Should only scan JS files for nodejs runtime
        js_files = [f for f in review_data["filesScanned"] if f.endswith('.js')]
        py_files = [f for f in review_data["filesScanned"] if f.endswith('.py')]
        
        self.assertGreater(len(js_files), 0)
        self.assertEqual(len(py_files), 0)  # Python files should not be scanned for nodejs

    def test_common_directory_exclusions(self):
        """Test that common directories are excluded for all runtimes"""
        # Test common exclusions for both runtimes
        for runtime in ["python3.11", "nodejs20"]:
            self.assertTrue(self.analyzer._should_skip_file("__pycache__/module.pyc", runtime))
            self.assertTrue(self.analyzer._should_skip_file(".git/config", runtime))
            self.assertTrue(self.analyzer._should_skip_file(".vscode/settings.json", runtime))
            self.assertTrue(self.analyzer._should_skip_file("dist/bundle.js", runtime))

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