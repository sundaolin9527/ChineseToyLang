# 定义接口库（必须在设置属性前创建目标）
add_library(IR_HEADERS INTERFACE)

# 设置头文件包含路径
target_include_directories(IR_HEADERS INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<INSTALL_INTERFACE:include>
    ${LLVM_INCLUDE_DIRS}
)

# 可选：添加LLVM相关定义
target_compile_definitions(IR_HEADERS INTERFACE ${LLVM_DEFINITIONS})

# 声明头文件（可选，用于IDE显示）
set_target_properties(IR_HEADERS PROPERTIES
    PUBLIC_HEADER "${IR_HEADERS}"
)
