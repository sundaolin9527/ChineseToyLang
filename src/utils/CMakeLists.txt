set(UTILS_SOURCES
    utils.c
    utils.h
)

add_library(utils STATIC ${UTILS_SOURCES})

# 导出头文件目录
target_include_directories(utils PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/src
)