add_library(Basic STATIC
    Diagnostics.c
    Utils.c
    Types.c
)
target_link_libraries(Basic PUBLIC ChineseToyLang_headers)