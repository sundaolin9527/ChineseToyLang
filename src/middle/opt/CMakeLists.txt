set(OPT_SOURCES
    optimizer.cpp
    optimizer.h
)

add_library(middle_opt STATIC ${OPT_SOURCES})

# 导出头文件目录
target_include_directories(middle_opt PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/src
)