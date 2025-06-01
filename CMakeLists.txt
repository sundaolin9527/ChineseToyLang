cmake_minimum_required(VERSION 3.15)
project(elang_core LANGUAGES C)  # 明确使用C语言

# 根目录 CMakeLists.txt
option(BUILD_LIB "Build runtime library" ON)
option(BUILD_TOOLS "Build auxiliary tools" ON)
option(BUILD_TESTS "Build tests" ON)

if(BUILD_LIB)
    add_subdirectory(lib)
endif()
if(BUILD_TOOLS)
    add_subdirectory(tools)
endif()
if(BUILD_TESTS)
    add_subdirectory(tests)
endif()

# 始终编译 src
add_subdirectory(src)