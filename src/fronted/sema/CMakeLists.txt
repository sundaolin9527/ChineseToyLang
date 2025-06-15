set(SEMA_SOURCES
    sema.c
    sema.h
)

add_library(fronted_sema STATIC ${SEMA_SOURCES})

# 导出头文件目录
target_include_directories(fronted_sema PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/src
)