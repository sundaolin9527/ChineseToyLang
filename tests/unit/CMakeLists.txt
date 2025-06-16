# 单元测试配置

# 查找所有测试源文件
file(GLOB TEST_SOURCES "*.c")

# 为每个测试源文件创建独立的测试可执行文件
foreach(test_source ${TEST_SOURCES})
    get_filename_component(test_name ${test_source} NAME_WE)
    
    # 创建测试可执行文件
    add_executable(${test_name} ${test_source})
    
    # 链接必要的库
    target_link_libraries(${test_name}
        utils
        datastructure
        fronted_lexer
        fronted_type
        fronted_parser
        fronted_ast
        fronted_sema
        fronted_infer
        middle_ir
        middle_opt
    )
    
    # 添加测试用例
    add_test(
        NAME ${test_name}
        COMMAND ${test_name}
    )
    
    # 设置测试工作目录
    set_property(TEST ${test_name} PROPERTY WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    
    # 添加头文件搜索路径
    target_include_directories(${test_name} PRIVATE
        ${CMAKE_SOURCE_DIR}/src
    )
endforeach()

# 新增测试目标
add_executable(test_ir test_ir.cpp)
target_include_directories(test_ir PRIVATE
    ${LLVM_INCLUDE_DIRS}
)
target_link_libraries(test_ir
    ${llvm_libs}
)