cmake_minimum_required(VERSION 3.16)
project(sma_bot VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required packages
find_package(CURL REQUIRED)

include(FetchContent)

# Fetch nlohmann/json
FetchContent_Declare(json
    GIT_REPOSITORY https://github.com/nlohmann/json.git
    GIT_TAG v3.11.3
)
FetchContent_MakeAvailable(json)

# Fetch the0 SDK
FetchContent_Declare(the0
    GIT_REPOSITORY https://github.com/alexanderwanyoike/the0.git
    GIT_TAG release/v1-1-0/fixes-3
    SOURCE_SUBDIR sdk/cpp
)
FetchContent_MakeAvailable(the0)

# Create executable
add_executable(sma_bot main.cpp)

# Link libraries
target_link_libraries(sma_bot PRIVATE
    the0
    CURL::libcurl
    nlohmann_json::nlohmann_json
)

# Install target
install(TARGETS sma_bot RUNTIME DESTINATION bin)
