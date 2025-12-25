cmake_minimum_required(VERSION 3.14)
project(the0_cpp_tests)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Fetch nlohmann/json
include(FetchContent)
FetchContent_Declare(json
    GIT_REPOSITORY https://github.com/nlohmann/json.git
    GIT_TAG v3.11.3
)
FetchContent_MakeAvailable(json)

# Include parent directory for the0.h
include_directories(${CMAKE_SOURCE_DIR}/..)

add_executable(the0_test the0_test.cpp)
target_link_libraries(the0_test PRIVATE nlohmann_json::nlohmann_json)
