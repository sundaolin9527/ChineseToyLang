add_library(Frontend STATIC
    Lexer.c
    Parser.c
    Sema.c
    Infer.c
    Ast.c
)
target_link_libraries(Frontend PUBLIC Basic)