set(IR_SOURCES
    ir.h
)

add_library(middle_ir STATIC ${IR_SOURCES})

# 导出头文件目录
target_include_directories(middle_ir PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/src
)