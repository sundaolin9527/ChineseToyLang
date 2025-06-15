set(TYPE_SOURCES
    type.c
    type.h
)

add_library(fronted_type STATIC ${TYPE_SOURCES})

# 导出头文件目录
target_include_directories(fronted_type PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/src
)