cmake_minimum_required(VERSION 3.10)
project(the0_cpp_tests)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Include parent directory for the0.h and json.hpp
include_directories(${CMAKE_SOURCE_DIR}/..)

add_executable(the0_test the0_test.cpp)
