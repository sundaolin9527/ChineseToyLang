add_library(IR STATIC
    # CodeGen.cpp
    Pass.cpp
)
target_link_libraries(IR PUBLIC Frontend)