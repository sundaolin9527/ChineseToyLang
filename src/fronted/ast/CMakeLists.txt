set(AST_SOURCES
    ast.c
    ast.h
)

add_library(fronted_ast STATIC ${AST_SOURCES})

# 导出头文件目录
target_include_directories(fronted_ast PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/src
)