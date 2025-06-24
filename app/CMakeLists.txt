# 可执行文件配置
add_executable(ChineseToyLang
    Main.cpp
)

# 版本信息配置
target_compile_definitions(ChineseToyLang PRIVATE
    PROJECT_VERSION="${PROJECT_VERSION}"
    BUILD_TIMESTAMP="${CMAKE_BUILD_TIMESTAMP}"
)

# 包含目录配置
target_include_directories(ChineseToyLang PRIVATE
    ${LLVM_INCLUDE_DIRS}
    ${CMAKE_SOURCE_DIR}/include
    ${zstd_INCLUDE_DIRS}
    ${CURL_INCLUDE_DIRS}
)

# 正确的链接配置
target_link_libraries(ChineseToyLang
    Frontend
    IR
    Backend_X86
    Backend_ARM
    Basic
    ${LLVM_LIBS}
    zstd::libzstd_shared
    ${CURL_LIBRARIES}
    $<$<PLATFORM_ID:Linux>:-rdynamic>
)

# 单元测试支持（可选）
if(BUILD_TESTING AND EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/tests")
    enable_testing()
    add_subdirectory(tests)
endif()