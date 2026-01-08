cmake_minimum_required(VERSION 3.16)
project(sma_bot VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)

# Fetch libcurl
set(BUILD_CURL_EXE OFF CACHE BOOL "" FORCE)
set(BUILD_SHARED_LIBS OFF CACHE BOOL "" FORCE)
set(CURL_DISABLE_TESTS ON CACHE BOOL "" FORCE)
set(HTTP_ONLY ON CACHE BOOL "" FORCE)
set(CURL_DISABLE_LDAP ON CACHE BOOL "" FORCE)
set(CURL_DISABLE_LDAPS ON CACHE BOOL "" FORCE)
FetchContent_Declare(curl
    GIT_REPOSITORY https://github.com/curl/curl.git
    GIT_TAG curl-8_5_0
    GIT_SHALLOW TRUE
)
FetchContent_MakeAvailable(curl)

# Fetch nlohmann/json
FetchContent_Declare(json
    GIT_REPOSITORY https://github.com/nlohmann/json.git
    GIT_TAG v3.11.3
)
FetchContent_MakeAvailable(json)

# Fetch the0 SDK
# TODO: Switch back to v1.3.0 tag when release merged
FetchContent_Declare(the0
    GIT_REPOSITORY https://github.com/alexanderwanyoike/the0.git
    GIT_TAG release/v1.3.0
    SOURCE_SUBDIR sdk/cpp
)
FetchContent_MakeAvailable(the0)

# Create executables
add_executable(sma_bot main.cpp)
add_executable(sma_query query.cpp)

# Link libraries
target_link_libraries(sma_bot PRIVATE
    the0
    libcurl
    nlohmann_json::nlohmann_json
)

target_link_libraries(sma_query PRIVATE
    the0
    nlohmann_json::nlohmann_json
)

# Install targets
install(TARGETS sma_bot sma_query RUNTIME DESTINATION bin)
