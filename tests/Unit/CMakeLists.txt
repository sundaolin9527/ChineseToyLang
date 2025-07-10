# Basic测试
add_executable(Basic_UtilsTest Basic/UtilsTest.cpp)
target_link_libraries(Basic_UtilsTest PRIVATE 
    Basic
    Frontend
)

add_executable(Basic_TypeTest Basic/TypeTest.cpp)
target_link_libraries(Basic_TypeTest PRIVATE 
    Basic
    Frontend
)

# Frontend测试
add_executable(Frontend_LexerTest Frontend/LexerTest.cpp)
target_link_libraries(Frontend_LexerTest PRIVATE
    Basic 
    Frontend
)

add_executable(Frontend_ParserTest Frontend/ParserTest.cpp)
target_link_libraries(Frontend_ParserTest PRIVATE
    Basic
    Frontend
)

# IR测试
add_executable(IR_CodeGenTest IR/CodeGenTest.cpp)
target_link_libraries(IR_CodeGenTest PRIVATE 
    Basic
    Frontend
    IR
)

add_executable(IR_PassTest IR/PassTest.cpp)
target_link_libraries(IR_PassTest PRIVATE 
    Basic
    Frontend
    IR
)