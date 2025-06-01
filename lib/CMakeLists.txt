cmake_minimum_required(VERSION 3.15)
project(elang_core LANGUAGES C)  # 明确使用C语言

# 标准库（如数学、IO等）
add_library(elang_stdlib STATIC
    io.c
    math.c
)
target_include_directories(elang_stdlib PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}
)

# 安装到系统目录
install(TARGETS elang_stdlib DESTINATION lib)