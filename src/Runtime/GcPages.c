#include <sys/mman.h>
#include <unistd.h>
#include "GcPages.h"

static int block_pg_cnt = DEFAULT_BLOCK_PG_ALLOC;
static size_t current_pg_count = 0;

void jl_gc_init_page(void)
{
    size_t jl_page_size = sysconf(_SC_PAGESIZE); // 默认系统页
    if (GC_PAGE_SZ * block_pg_cnt < jl_page_size)
    {
        block_pg_cnt = jl_page_size / GC_PAGE_SZ;
    }
}

static char *gc_try_alloc_pages(int pg_cnt)
{
    size_t jl_page_size = sysconf(_SC_PAGESIZE);
    size_t pages_sz = GC_PAGE_SZ * pg_cnt;
    if (GC_PAGE_SZ > jl_page_size)
        pages_sz += GC_PAGE_SZ;
    char *mem = (char*)mmap(0, pages_sz, PROT_READ | PROT_WRITE,
                            MAP_NORESERVE | MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mem == MAP_FAILED)
        return NULL;
    if (GC_PAGE_SZ > jl_page_size)
        mem = (char*)gc_page_data(mem + GC_PAGE_SZ - 1);
    return mem;
}

gc_pagemeta_t* get_page_metadata(pagetable_t *memory_map, void *_data)
{
    uintptr_t data = ((uintptr_t)_data);
    unsigned i;
    i = REGION_INDEX(data);
    pagetable1_t *r1 = memory_map->meta1[i];
    if (!r1)
        return NULL;
    i = REGION1_INDEX(data);
    pagetable0_t *r0 = r1->meta0[i];
    if (!r0)
        return NULL;
    i = REGION0_INDEX(data);
    return r0->meta[i];
}
