# 只有头文件的库
add_library(datastructure INTERFACE)

# 导出头文件目录
target_include_directories(datastructure INTERFACE 
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/src
)