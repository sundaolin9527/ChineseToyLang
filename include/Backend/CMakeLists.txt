target_include_directories(ChineseToyLang_headers INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/X86>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/ARM>
)

# 主后端头文件
set(BACKEND_HEADERS
    CodeGen.h
)


# 平台特定配置
add_subdirectory(X86)
add_subdirectory(ARM)