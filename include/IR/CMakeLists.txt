target_include_directories(ChineseToyLang_headers INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

set(IR_HEADERS
    IREmitter.h
    Pass.h
)
