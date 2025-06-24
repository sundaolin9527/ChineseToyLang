add_library(Backend_X86 STATIC
    X86CodeGen.cpp
)
target_link_libraries(Backend_X86 PRIVATE Backend_common)