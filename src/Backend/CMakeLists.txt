# 公共后端
add_library(Backend_common STATIC
    CodeGen.cpp
)

# 平台后端
add_subdirectory(X86)
add_subdirectory(ARM)