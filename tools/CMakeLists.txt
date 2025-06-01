cmake_minimum_required(VERSION 3.15)
project(elang_core LANGUAGES C)  # 明确使用C语言

# 代码格式化工具
add_executable(elang-format
    fmt/main.c
    fmt/formatter.c
)
target_link_libraries(elang-format PRIVATE frontend)

# 语言服务器（LSP）
add_executable(lsp
    lsp/server.c
    lsp/protocol.c
)
target_link_libraries(lsp PRIVATE frontend)

# 安装工具到系统路径
install(TARGETS elang-format lsp DESTINATION bin)