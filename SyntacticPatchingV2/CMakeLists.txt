cmake_minimum_required(VERSION 3.0)

set(CMAKE_CXX_STANDARD 11)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
SET(CMAKE_EXPORT_COMPILE_COMMANDS ON)
find_package (Threads)
link_libraries(${CMAKE_THREAD_LIBS_INIT})


# llvm
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMCOnfig.cmake in: ${LLVM_DIR}")
add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})

link_libraries(LLVM)

# clang
find_package(Clang REQUIRED CONFIG)
# linking
link_libraries(clang clangTooling clangFrontend clangFrontendTool clangAST clangBasic)




add_executable(tool tooling.cpp)
# plugin
add_library(helium SHARED plugin.cpp sexp.cpp)
