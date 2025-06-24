cmake_minimum_required(VERSION 3.15)
project(ChineseToyLang LANGUAGES C CXX)

# 编译器设置
set(CMAKE_C_COMPILER "clang-16")
set(CMAKE_CXX_COMPILER "clang++-16")

# 标准设置
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_BUILD_TYPE Debug)

# 输出目录
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# 依赖检查
find_package(LLVM 16 REQUIRED CONFIG)
find_package(zstd REQUIRED)
find_package(CURL REQUIRED)
if(NOT LLVM_PACKAGE_VERSION VERSION_EQUAL "16.0.6")
    message(WARNING "LLVM 16.0.6 recommended, found ${LLVM_PACKAGE_VERSION}")
endif()

# 子目录
add_subdirectory(include)
add_subdirectory(src)
add_subdirectory(app)

if(BUILD_TESTING)
    enable_testing()
    add_subdirectory(test)
endif()
