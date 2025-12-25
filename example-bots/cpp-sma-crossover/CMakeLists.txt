cmake_minimum_required(VERSION 3.16)
project(sma_bot VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required packages
find_package(CURL REQUIRED)

# Fetch nlohmann/json
include(FetchContent)
FetchContent_Declare(
    json
    GIT_REPOSITORY https://github.com/nlohmann/json.git
    GIT_TAG v3.11.3
)
FetchContent_MakeAvailable(json)

# Create executable
add_executable(sma_bot main.cpp)

# Link libraries
target_link_libraries(sma_bot PRIVATE
    CURL::libcurl
    nlohmann_json::nlohmann_json
)

# Install target
install(TARGETS sma_bot RUNTIME DESTINATION bin)
