cmake_minimum_required(VERSION 3.14)
project(the0_cpp_tests)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)

# Fetch nlohmann/json
FetchContent_Declare(json
    GIT_REPOSITORY https://github.com/nlohmann/json.git
    GIT_TAG v3.11.3
)

# Fetch doctest
FetchContent_Declare(doctest
    GIT_REPOSITORY https://github.com/doctest/doctest.git
    GIT_TAG v2.4.11
)

FetchContent_MakeAvailable(json doctest)

# Include parent directory for the0.h
include_directories(${CMAKE_SOURCE_DIR}/..)

add_executable(the0_test the0_test.cpp)
target_link_libraries(the0_test PRIVATE nlohmann_json::nlohmann_json doctest::doctest)
