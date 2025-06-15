cmake_minimum_required(VERSION 3.15)
project(ChineseToyLang LANGUAGES C CXX)

# 设置C和C++标准
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_BUILD_TYPE Debug)

# 设置输出目录
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# 查找 LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# 添加 LLVM 定义和包含目录
add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})

# 映射 LLVM 组件
llvm_map_components_to_libnames(llvm_libs
    Core
    ExecutionEngine
    IRReader
    Support
    native
    bitreader
    bitwriter
)

# 添加全局包含路径
include_directories(${CMAKE_SOURCE_DIR}/src)

# 添加 src 子目录
add_subdirectory(src)

# 添加测试目录
if(BUILD_TESTING)
    add_subdirectory(tests)
endif()

# 添加主程序
add_executable(ChineseToyLang main.c)

# 链接所有库到主程序
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
)

# 可选：生成编译器版本信息
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/version.h.in
    ${CMAKE_CURRENT_BINARY_DIR}/version.h
)