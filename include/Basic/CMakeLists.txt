# 添加模块特定包含路径
target_include_directories(ChineseToyLang_headers INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

# 显式声明头文件
set(BASIC_HEADERS
    Utils.h
    Diagnostics.h
    LinkedList.h
    Types.h
)
