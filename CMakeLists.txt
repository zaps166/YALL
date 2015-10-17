cmake_minimum_required(VERSION 2.8.8)
project(YaLL)

SET(CMAKE_BUILD_TYPE Release)
SET(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -s")

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS} -std=c++11)

add_executable(YaLL YaLL.cpp YaLLCompiler.cpp)

llvm_map_components_to_libnames(llvm_libs mcjit x86codegen interpreter)

target_link_libraries(YaLL ${llvm_libs})
