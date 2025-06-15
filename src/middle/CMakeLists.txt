# 添加中端模块子目录
add_subdirectory(ir)
add_subdirectory(opt)

# 定义中端模块依赖关系
target_link_libraries(middle_ir 
    fronted_ast 
    utils
    datastructure
    ${llvm_libs}
)
target_link_libraries(middle_opt 
    middle_ir 
    ${llvm_libs}
)