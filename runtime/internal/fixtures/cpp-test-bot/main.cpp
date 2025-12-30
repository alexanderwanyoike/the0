#include <cstdlib>
#include <iostream>
#include <string>

// Simple test bot without SDK dependency for runtime testing
// Real bots should use the0.h SDK

int main() {
    // Get BOT_ID and BOT_CONFIG from environment
    const char* bot_id_env = std::getenv("BOT_ID");
    const char* config_env = std::getenv("BOT_CONFIG");

    std::string bot_id = bot_id_env ? bot_id_env : "unknown";
    std::string config = config_env ? config_env : "{}";

    // Print inputs to stderr for test verification
    std::cerr << "BOT_ID: " << bot_id << std::endl;
    std::cerr << "BOT_CONFIG: " << config << std::endl;

    // Output success JSON to stdout (properly escaped)
    // Note: For production, use the0.h SDK which handles escaping correctly
    std::cout << "{\"status\":\"success\",\"message\":\"C++ bot executed\",\"bot_id\":\"" << bot_id << "\"}" << std::endl;

    return 0;
}
