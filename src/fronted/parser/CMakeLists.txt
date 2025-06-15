set(PARSER_SOURCES
    parser.c
    parser.h
)

add_library(fronted_parser STATIC ${PARSER_SOURCES})

# 导出头文件目录
target_include_directories(fronted_parser PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/src
)