cmake_minimum_required(VERSION 3.15)
project(elang_core LANGUAGES C)  # 明确使用C语言

# 启用测试框架
enable_testing()

# 单元测试
add_executable(test_lexer
    unit/test_lexer.c
)
target_link_libraries(test_lexer PRIVATE frontend)
add_test(NAME test_lexer COMMAND test_lexer)

# 集成测试（编译并运行示例）
add_test(NAME test_integration 
    COMMAND ${CMAKE_BINARY_DIR}/src/elang ${CMAKE_CURRENT_SOURCE_DIR}/examples/hello.elan
)

# 示例代码作为测试数据
file(COPY examples DESTINATION ${CMAKE_CURRENT_BINARY_DIR})