# 添加所有前端模块子目录
add_subdirectory(lexer)
add_subdirectory(type)
add_subdirectory(parser)
add_subdirectory(ast)
add_subdirectory(sema)
add_subdirectory(infer)

# 定义前端模块间的依赖关系
target_link_libraries(fronted_parser 
    fronted_lexer 
    fronted_type
    utils
    datastructure
)
target_link_libraries(fronted_ast fronted_parser)
target_link_libraries(fronted_sema fronted_ast)
target_link_libraries(fronted_infer fronted_sema)