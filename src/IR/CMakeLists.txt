add_library(IR STATIC
    # IREmitter.cpp
    Pass.cpp
)
target_link_libraries(IR PUBLIC Frontend)