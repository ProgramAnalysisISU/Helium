cmake_minimum_required(VERSION 3.6)
project(helium)

set(CMAKE_CXX_STANDARD 11)

set(CMAKE_BUILD_TYPE Debug)
# set(CMAKE_CXX_FLAGS "-O0 -pg -g")
# set(CMAKE_CXX_FLAGS "-pg -g")

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -g")
SET(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -g")

# https://github.com/ninja-build/ninja/wiki/FAQ
# ninja runs in parallel, so buffer the output for each thread
# clang would detect that its output is not directed to a terminal
# thus do not print the color
# so force it to use color
# Add_compile_options(-fcolor-diagnostics)
Add_compile_options(-g)

# remove this to avoid profiling overhead
# Add_compile_options(-pg)
# Add_compile_options(-lprofiler)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

SET(CMAKE_EXPORT_COMPILE_COMMANDS ON)

enable_testing()
find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

find_package (Threads)
link_libraries(${CMAKE_THREAD_LIBS_INIT})

find_package( Boost 1.40 COMPONENTS system filesystem program_options regex timer REQUIRED )
include_directories(${Boost_INCLUDE_DIRS})


## Setting LLVM
find_package(LLVM REQUIRED CONFIG)
# message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
# message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
# message(STATUS "LLVM include: ${LLVM_INCLUDE_DIRS}")
# message(STATUS "LLVM library: ${LLVM_LIBRARY_DIRS}")
add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})

link_libraries(LLVM)


## Setting Clang
# this is required
find_package(Clang REQUIRED CONFIG)
# message(STATUS "Found Clang ${Clang_PACKAGE_VERSION}")
# message(STATUS "Using ClangConfig.cmake in: ${CLANG_INSTALL_PREFIX}")
# message(STATUS "Clang include: ${Clang_INCLUDE_DIRS}")
# message(STATUS "Clang library: ${Clang_LIBRARY_DIRS}")

# why I need this??? Otherwise error: undefined reference to typeinfo for xxx
# add_compile_options(-fno-rtti)

# link these by default
link_libraries(clang clangTooling clangFrontend clangFrontendTool clangAST clangBasic clangLex clangRewrite)



include_directories(include)

add_subdirectory(src)
add_subdirectory(test)
# add_subdirectory(tools)

find_package(Doxygen)
if((DOXYGEN_FOUND))
  set(doxyfile_in ${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in)
  set(doxyfile ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)
  configure_file(${doxyfile_in} ${doxyfile} @ONLY)
  add_custom_target(doc
    COMMAND ${DOXYGEN_EXECUTABLE} ${doxyfile}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generating API documentation with Doxygen"
    VERBATIM)
endif()
  
