target_include_directories(ChineseToyLang_headers INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

set(FRONTEND_HEADERS
    Lexer.h
    Parser.h
    Sema.h
    Infer.h
    Ast.h
)
