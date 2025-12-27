/**
 * Test utilities for the0 C++ SDK tests
 */

#ifndef THE0_TEST_HELPERS_HPP
#define THE0_TEST_HELPERS_HPP

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>
#include <iostream>

namespace the0_test {

/**
 * RAII helper to set an environment variable and restore on destruction
 */
class SetEnv {
public:
    SetEnv(const char* name, const char* value) : name_(name) {
        setenv(name, value, 1);
    }
    ~SetEnv() {
        unsetenv(name_);
    }

    // Non-copyable
    SetEnv(const SetEnv&) = delete;
    SetEnv& operator=(const SetEnv&) = delete;

private:
    const char* name_;
};

/**
 * RAII helper to capture stdout for metric tests
 */
class CaptureStdout {
public:
    CaptureStdout() : old_buf_(std::cout.rdbuf(buffer_.rdbuf())) {}
    ~CaptureStdout() { std::cout.rdbuf(old_buf_); }

    std::string get() const { return buffer_.str(); }

    // Non-copyable
    CaptureStdout(const CaptureStdout&) = delete;
    CaptureStdout& operator=(const CaptureStdout&) = delete;

private:
    std::stringstream buffer_;
    std::streambuf* old_buf_;
};

/**
 * RAII helper to capture stderr for log tests
 */
class CaptureStderr {
public:
    CaptureStderr() : old_buf_(std::cerr.rdbuf(buffer_.rdbuf())) {}
    ~CaptureStderr() { std::cerr.rdbuf(old_buf_); }

    std::string get() const { return buffer_.str(); }

    // Non-copyable
    CaptureStderr(const CaptureStderr&) = delete;
    CaptureStderr& operator=(const CaptureStderr&) = delete;

private:
    std::stringstream buffer_;
    std::streambuf* old_buf_;
};

/**
 * RAII helper to set up a temp directory for result files
 *
 * Sets CODE_MOUNT_DIR to a temporary directory, creating result.json
 * and cleaning up on destruction.
 */
class ResultFileContext {
public:
    ResultFileContext() {
        // Create temp directory
        temp_dir_ = std::filesystem::temp_directory_path() / ("the0_test_" + std::to_string(rand()));
        std::filesystem::create_directories(temp_dir_);

        // Set environment variable (the0 SDK reads CODE_MOUNT_DIR)
        setenv("CODE_MOUNT_DIR", temp_dir_.c_str(), 1);
    }

    ~ResultFileContext() {
        // Unset env
        unsetenv("CODE_MOUNT_DIR");

        // Cleanup temp directory
        try {
            std::filesystem::remove_all(temp_dir_);
        } catch (...) {
            // Ignore cleanup errors in destructor
        }
    }

    /**
     * Get the path to the result file
     */
    std::string result_file_path() const {
        return "/" + temp_dir_.string() + "/result.json";
    }

    /**
     * Read the contents of the result file
     */
    std::string read_result() const {
        std::ifstream f(result_file_path());
        if (!f.is_open()) {
            return "";
        }
        std::stringstream buffer;
        buffer << f.rdbuf();
        return buffer.str();
    }

    /**
     * Check if result file exists
     */
    bool result_exists() const {
        return std::filesystem::exists(result_file_path());
    }

    // Non-copyable
    ResultFileContext(const ResultFileContext&) = delete;
    ResultFileContext& operator=(const ResultFileContext&) = delete;

private:
    std::filesystem::path temp_dir_;
};

} // namespace the0_test

#endif // THE0_TEST_HELPERS_HPP
