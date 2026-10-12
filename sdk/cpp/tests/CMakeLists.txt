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

# Automatically find all test source files
# main.cpp provides the doctest entry point
# test_*.cpp files contain the actual tests
file(GLOB TEST_SOURCES
    "${CMAKE_SOURCE_DIR}/main.cpp"
    "${CMAKE_SOURCE_DIR}/test_*.cpp"
)

# Log discovered test files for debugging
message(STATUS "Discovered test files:")
foreach(TEST_FILE ${TEST_SOURCES})
    message(STATUS "  - ${TEST_FILE}")
endforeach()

add_executable(the0_test ${TEST_SOURCES})
target_link_libraries(the0_test PRIVATE nlohmann_json::nlohmann_json doctest::doctest)

# Enable CTest integration
enable_testing()
add_test(NAME the0_sdk_tests COMMAND the0_test)

# The bot contract cases (sdk/contract/README.md). The harness runs the
# contract bot once per case; ctest gets one test per case file.
find_package(Threads REQUIRED)

get_filename_component(THE0_CONTRACT_CASES_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../contract/cases" REALPATH)
set(THE0_CPP_CASES_DIR "${CMAKE_CURRENT_SOURCE_DIR}/contract/cases")

add_executable(the0_contract_bot contract/contract_bot.cpp)
target_link_libraries(the0_contract_bot PRIVATE nlohmann_json::nlohmann_json)

add_executable(the0_contract_test contract/harness.cpp)
target_link_libraries(the0_contract_test PRIVATE nlohmann_json::nlohmann_json Threads::Threads)
target_compile_definitions(the0_contract_test PRIVATE
    THE0_CONTRACT_BOT="$<TARGET_FILE:the0_contract_bot>"
    THE0_CONTRACT_CASES_DIR="${THE0_CONTRACT_CASES_DIR}"
    THE0_CPP_CASES_DIR="${THE0_CPP_CASES_DIR}"
)
add_dependencies(the0_contract_test the0_contract_bot)

add_test(NAME contract/known-failures COMMAND the0_contract_test --check-known-failures)

file(GLOB THE0_CONTRACT_CASES CONFIGURE_DEPENDS "${THE0_CONTRACT_CASES_DIR}/*.json")
foreach(CASE_FILE ${THE0_CONTRACT_CASES})
    get_filename_component(CASE_NAME ${CASE_FILE} NAME_WE)
    add_test(NAME contract/${CASE_NAME} COMMAND the0_contract_test ${CASE_FILE})
endforeach()

# Cases in the contract's format that only this SDK runs.
file(GLOB THE0_CPP_CASES CONFIGURE_DEPENDS "${THE0_CPP_CASES_DIR}/*.json")
foreach(CASE_FILE ${THE0_CPP_CASES})
    get_filename_component(CASE_NAME ${CASE_FILE} NAME_WE)
    add_test(NAME contract-cpp/${CASE_NAME} COMMAND the0_contract_test ${CASE_FILE})
endforeach()
