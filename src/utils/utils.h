#ifndef UTILS_H
#define UTILS_H

char* read_file(const char* filename);

// 双向循环链表节点结构
typedef struct Node Node;
struct Node {
    Node *next;
    Node *prev;
};

// 初始化动态分配的链表节点
#define INIT_LIST_HEAD(ptr) \
    do { \
        ((ptr)->next) = (ptr); \
        ((ptr)->prev) = (ptr); \
    } while (0)

// 在头部插入节点（需确保 head 已初始化）
#define LIST_INSERT_HEAD(head, newNode) \
    do { \
        ((newNode)->next) = ((head)->next); \
        ((newNode)->prev) = (head); \
        ((head)->next->prev) = (newNode); \
        ((head)->next) = (newNode); \
    } while (0)

// 在尾部插入节点（需确保 head 已初始化）
#define LIST_INSERT_TAIL(head, newNode) \
    do { \
        ((newNode)->next) = (head); \
        ((newNode)->prev) = ((head)->prev); \
        ((head)->prev->next) = (newNode); \
        ((head)->prev) = (newNode); \
    } while (0)

// 删除节点
#define LIST_DELETE(node) \
    do { \
        ((node)->prev->next) = ((node)->next); \
        ((node)->next->prev) = ((node)->prev); \
        ((node)->next) = (node); \
        ((node)->prev) = (node); \
    } while (0)

/**释放节点 */
#define LIST_FREE(head, type, node) \
do { \
    Node *pos, *next; \
    type *temp = NULL; \
    LIST_FOREACH_SAFE(pos, next, &(head->node)) \
    { \
        LIST_DELETE(pos);   \
        temp = LIST_ENTRY(pos, type, node); \
        free(temp); \
        temp = NULL; \
    } \
    free(head); \
} while (0)

// 安全遍历宏
#define LIST_FOREACH_SAFE(pos, n, head) \
    for (((pos) = ((head)->next)), ((n) = ((pos)->next)); ((pos) != (head)); ((pos) = (n)), ((n) = ((pos)->next)))

// 获取外层结构体指针
#define LIST_ENTRY(ptr, type, member) \
    ((type *)((char *)(ptr) - (unsigned long)(&((type *)0)->member)))

#define LIST_IS_EMPTY(head) (((head)->next) == (head))

// 删除头部节点（返回被删除的节点）
#define LIST_POP_HEAD(head) \
    ({ \
        Node *popped = (head)->next; \
        if (popped != (head)) { \
            (head)->next = popped->next; \
            popped->next->prev = (head); \
            popped->next = popped; \
            popped->prev = popped; \
        } else { \
            popped = NULL; \
        } \
        popped; \
    })
#endif /* UTILS_H */ 