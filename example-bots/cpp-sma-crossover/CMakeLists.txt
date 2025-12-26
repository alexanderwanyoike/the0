cmake_minimum_required(VERSION 3.16)
project(sma_bot VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required packages
find_package(CURL REQUIRED)

include(FetchContent)

# Fetch the0 SDK
FetchContent_Declare(
    the0_sdk
    GIT_REPOSITORY https://github.com/alexanderwanyoike/the0.git
    GIT_TAG release/v1.1.0
    SOURCE_SUBDIR sdk/cpp
)
FetchContent_MakeAvailable(the0_sdk)

# Fetch nlohmann/json
FetchContent_Declare(
    json
    GIT_REPOSITORY https://github.com/nlohmann/json.git
    GIT_TAG v3.11.3
)
FetchContent_MakeAvailable(json)

# Create executable
add_executable(sma_bot main.cpp)

# Include the0 SDK header
target_include_directories(sma_bot PRIVATE ${the0_sdk_SOURCE_DIR})

# Link libraries
target_link_libraries(sma_bot PRIVATE
    CURL::libcurl
    nlohmann_json::nlohmann_json
)

# Install target
install(TARGETS sma_bot RUNTIME DESTINATION bin)
