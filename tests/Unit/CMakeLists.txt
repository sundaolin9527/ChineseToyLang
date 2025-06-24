# Basic测试
add_executable(Basic_UtilsTest Basic/UtilsTest.cpp)
target_link_libraries(Basic_UtilsTest PRIVATE Basic)

add_executable(Basic_TypeTest Basic/TypeTest.cpp)
target_link_libraries(Basic_TypeTest PRIVATE Basic)

# Frontend测试
add_executable(Frontend_LexerTest Frontend/LexerTest.cpp)
target_link_libraries(Frontend_LexerTest PRIVATE Frontend)

add_executable(Frontend_ParserTest Frontend/ParserTest.cpp)
target_link_libraries(Frontend_ParserTest PRIVATE Frontend)

# IR测试
add_executable(IR_BuilderTest IR/IRBuilderTest.cpp)
target_link_libraries(IR_BuilderTest PRIVATE IR)

add_executable(IR_ModuleTest IR/ModuleTest.cpp)
target_link_libraries(IR_ModuleTest PRIVATE IR)