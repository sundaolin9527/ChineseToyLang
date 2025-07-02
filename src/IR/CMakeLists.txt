# 合并两个 add_library 调用
add_library(IR STATIC
    #CodeGen.cpp
    Pass.cpp
)

# 同时链接 Frontend 和 LLVM 库
target_link_libraries(IR 
    PUBLIC 
        Frontend  # 你的项目内部依赖
    PRIVATE 
        ${LLVM_LIBS}  # LLVM 私有依赖
)