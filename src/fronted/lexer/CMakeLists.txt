set(LEXER_SOURCES
    lexer.c
    lexer.h
)

add_library(fronted_lexer STATIC ${LEXER_SOURCES})

# 导出头文件目录
target_include_directories(fronted_lexer PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/src
)