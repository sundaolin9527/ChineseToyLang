# 头文件库配置 (INTERFACE 目标)
add_library(ChineseToyLang_headers INTERFACE)

# 包含所有头文件目录
target_include_directories(ChineseToyLang_headers INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<INSTALL_INTERFACE:include>
)

# 添加子目录
add_subdirectory(Basic)
add_subdirectory(Frontend)
add_subdirectory(IR)
add_subdirectory(Backend)