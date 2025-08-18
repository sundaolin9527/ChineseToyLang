#ifndef GC_H
#define GC_H
#ifdef __cplusplus
extern "C" {
#endif
#include <stdint.h>
#include <string.h>

typedef enum {
    GC_AUTO = 0,         // 自适应
    GC_FULL = 1,         // 全量gc
    GC_INCREMENTAL = 2,  // 增量gc
} gc_collection_t;

#ifdef __cplusplus
}
#endif
#endif