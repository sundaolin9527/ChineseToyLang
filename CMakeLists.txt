cmake_minimum_required(VERSION 3.15)
project(ChineseToyLang 
    LANGUAGES C CXX
    VERSION 0.0.1
)

# 编译器设置
set(CMAKE_C_COMPILER "clang-17")
set(CMAKE_CXX_COMPILER "clang++-17")

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
find_package(LLVM 17 REQUIRED CONFIG)
find_package(zstd REQUIRED)
find_package(CURL REQUIRED)
if(NOT LLVM_PACKAGE_VERSION VERSION_EQUAL "17.0.6")
    message(WARNING "LLVM 17.0.6 recommended, found ${LLVM_PACKAGE_VERSION}")
endif()

# LLVM 组件
llvm_map_components_to_libnames(llvm_libs
    Core
    ExecutionEngine
    IRReader
    Support
    BitReader
    BitWriter
)

# 添加LLVM头文件路径
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# 子目录
add_subdirectory(src)
if(BUILD_TESTING)
    add_subdirectory(tests)
endif()

# 主程序
add_executable(ChineseToyLang main.c)
target_include_directories(ChineseToyLang PRIVATE
    ${LLVM_INCLUDE_DIRS}
    ${CMAKE_SOURCE_DIR}/src
)
target_compile_definitions(ChineseToyLang PRIVATE 
    ${LLVM_DEFINITIONS}
)

target_link_libraries(ChineseToyLang
    utils
    datastructure
    fronted_lexer
    fronted_type
    fronted_parser
    fronted_ast
    fronted_sema
    fronted_infer
    middle_ir
    middle_opt
    ${llvm_libs}
    zstd::libzstd_shared
    ${CURL_LIBRARIES}
)