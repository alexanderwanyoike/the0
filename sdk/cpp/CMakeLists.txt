cmake_minimum_required(VERSION 3.14)
project(the0-sdk VERSION 0.1.0 LANGUAGES CXX)

add_library(the0 INTERFACE)
target_include_directories(the0 INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_features(the0 INTERFACE cxx_std_17)

# Require nlohmann/json
find_package(nlohmann_json QUIET)
if(nlohmann_json_FOUND)
    target_link_libraries(the0 INTERFACE nlohmann_json::nlohmann_json)
endif()
