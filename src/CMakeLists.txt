cmake_minimum_required(VERSION 3.15)
project(elang_core LANGUAGES C)

set(ELANG_VERSION "1.0.0")
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# 前端模块（词法/语法/语义分析）
add_library(frontend STATIC
    frontend/lexer/lexer.c
    frontend/parser/parser.c
    frontend/ast/ast.c
    frontend/sema/sema.c
)

target_include_directories(frontend PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}/frontend
)

# 工具
add_library(utils STATIC
    utils/utils.c
)

target_include_directories(utils PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}/utils
)

# 驱动程序和主入口
add_executable(ChineseToyLang
    main.c
)
target_link_libraries(ChineseToyLang PRIVATE frontend utils)

# 可选：生成编译器版本信息
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/version.h.in
    ${CMAKE_CURRENT_BINARY_DIR}/version.h
)