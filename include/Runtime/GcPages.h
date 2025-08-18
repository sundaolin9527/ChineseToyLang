#ifndef GC_H
#define GC_H
#ifdef __cplusplus
extern "C" {
#endif

#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <inttypes.h>
#include "Gc.h"

#define DEFAULT_BLOCK_PG_ALLOC (4096) // 64 MB
#define MIN_BLOCK_PG_ALLOC (1) // 16 KB

#define GC_HEAP_ALIGNMENT 16
#define GC_PAGE_LG2 14
#define GC_PAGE_SZ (1 << GC_PAGE_LG2) // 16k
#define GC_PAGE_OFFSET (GC_HEAP_ALIGNMENT - (sizeof(taggedvalue_t) % GC_HEAP_ALIGNMENT))

#define REGION0_PG_COUNT (1 << 16)
#define REGION1_PG_COUNT (1 << 16)
#define REGION2_PG_COUNT (1 << 18)
#define REGION0_INDEX(p) (((uintptr_t)(p) >> 14) & 0xFFFF) // 右移 GC_PAGE_LG2
#define REGION1_INDEX(p) (((uintptr_t)(p) >> 30) & 0xFFFF)
#define REGION_INDEX(p)  (((uintptr_t)(p) >> 46) & 0x3FFFF)

// 元数据
typedef struct {
    uint8_t pool_n;
    uint8_t has_marked;
    uint8_t has_young;
    uint16_t nold;
    uint16_t prev_nold;
    uint16_t nfree;
    uint16_t osize;
    uint16_t fl_begin_offset;
    uint16_t fl_end_offset;
    uint16_t thread_n;
    char *data;
    uint8_t *ages;
} gc_pagemeta_t;

typedef struct {
    gc_pagemeta_t *meta[REGION0_PG_COUNT];
    uint32_t allocmap[REGION0_PG_COUNT / 32];
    uint32_t freemap[REGION0_PG_COUNT / 32];
    int lb;
    int ub;
} pagetable0_t;

typedef struct {
    pagetable0_t *meta0[REGION1_PG_COUNT];
    uint32_t allocmap0[REGION1_PG_COUNT / 32];
    uint32_t freemap0[REGION1_PG_COUNT / 32];
    int lb;
    int ub;
} pagetable1_t;

typedef struct {
    pagetable1_t *meta1[REGION2_PG_COUNT];
    uint32_t allocmap1[REGION2_PG_COUNT / 32];
    uint32_t freemap1[REGION2_PG_COUNT / 32];
    int lb;
    int ub;
} pagetable_t;

struct _taggedvalue_bits {
    uintptr_t gc:2;
};

typedef struct _value_t value_t;
typedef struct _taggedvalue_t taggedvalue_t;
struct _taggedvalue_t {
    union {
        uintptr_t header;
        taggedvalue_t *next;
        value_t *type; // 16-byte 对齐
        struct _taggedvalue_bits bits;
    };
    // value_t value;
};

static inline unsigned ffs_u32(uint32_t bitvec)
{
    return __builtin_ffs(bitvec) - 1;
}

static inline char *gc_page_data(void *x)
{
    return (char*)(((uintptr_t)x >> GC_PAGE_LG2) << GC_PAGE_LG2);
}

#ifdef __cplusplus
}
#endif
#endif