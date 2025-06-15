set(INFER_SOURCES
    infer.c
    infer.h
)

add_library(fronted_infer STATIC ${INFER_SOURCES})

# 导出头文件目录
target_include_directories(fronted_infer PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/src
)