add_library(Backend_ARM STATIC
    ARMCodeGen.cpp
)
target_link_libraries(Backend_ARM PRIVATE Backend_common)