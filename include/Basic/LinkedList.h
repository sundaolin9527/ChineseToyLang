#ifndef LINKEDLIST_H
#define LINKEDLIST_H
#ifdef __cplusplus
extern "C" {
#endif
/* 双向循环链表节点结构 
 * @member next 指向下一个节点的指针
 * @member prev 指向前一个节点的指针
 */
typedef struct Node Node;
struct Node {
    Node *next;
    Node *prev;
};

/**
 * 初始化链表头节点（创建空链表）
 * @param ptr 要初始化的头节点指针
 * @note 会使头节点的next和prev都指向自己，形成空链表
 */
#define INIT_LIST_HEAD(ptr) \
    do { \
        ((ptr)->next) = (ptr); \
        ((ptr)->prev) = (ptr); \
    } while (0)

/**
 * 在链表头部插入新节点
 * @param head 链表头节点（必须已初始化）
 * @param newNode 要插入的新节点
 * @note 插入后新节点成为链表的第一个元素
 */
#define LIST_INSERT_HEAD(head, newNode) \
    do { \
        ((newNode)->next) = ((head)->next); \
        ((newNode)->prev) = (head); \
        ((head)->next->prev) = (newNode); \
        ((head)->next) = (newNode); \
    } while (0)

/**
 * 在链表尾部插入新节点
 * @param head 链表头节点（必须已初始化）
 * @param newNode 要插入的新节点
 * @note 插入后新节点成为链表的最后一个元素
 */
#define LIST_INSERT_TAIL(head, newNode) \
    do { \
        ((newNode)->next) = (head); \
        ((newNode)->prev) = ((head)->prev); \
        ((head)->prev->next) = (newNode); \
        ((head)->prev) = (newNode); \
    } while (0)

/**
 * 从链表中删除节点
 * @param node 要删除的节点指针
 * @note 删除后该节点的next和prev会指向自己
 * @note 不会释放节点内存，只是从链表中断开
 */
#define LIST_DELETE(node) \
    do { \
        ((node)->prev->next) = ((node)->next); \
        ((node)->next->prev) = ((node)->prev); \
        ((node)->next) = (node); \
        ((node)->prev) = (node); \
    } while (0)

/*
 * @param member type结构体中Node成员的名称
 * @note 会遍历整个链表并释放所有节点内存
 * @note 最后会释放头节点内存
 * @warning 调用后整个链表将不可用
 */
#define LIST_FREE(head, type, member) \
do { \
    type *temp = NULL; /* 用于保存实际结构体指针 */ \
    Node *pos, *next;  /* 遍历用临时指针 */ \
    /* 安全遍历链表（允许在遍历时删除节点） */ \
    LIST_FOREACH_SAFE(head, pos, next) \
    { \
        LIST_DELETE(pos);   /* 先从链表中移除节点 */ \
        temp = LIST_ENTRY(pos, type, member); /* 获取包含该节点的实际结构体 */ \
        free(temp); /* 释放实际结构体内存 */ \
        temp = NULL; /* 防止野指针 */ \
    } \
    INIT_LIST_HEAD(head); \
} while (0)

/**
 * 从前往后安全遍历双向循环链表（支持遍历时删除节点）
 * @param head 链表头节点
 * @param pos 当前节点指针（Node*类型）
 * @param nextpos 临时变量用于保存后一个节点（Node*类型）
 */
#define LIST_FOREACH_SAFE(head, pos, nextpos) \
    for (((pos) = ((head)->next)), ((nextpos) = ((pos)->next));  \
        ((pos) != (head));  \
        ((pos) = (nextpos)), ((nextpos) = ((pos)->next))) \

/**
 * 从后往前安全遍历双向循环链表（支持遍历时删除节点）
 * @param head 链表头节点
 * @param pos 当前节点指针（Node*类型）
 * @param prevpos 临时变量用于保存前一个节点（Node*类型）
 */
#define LIST_FOREACH_REVERSE_SAFE(head, pos, prevpos) \
    for ((pos) = ((head)->prev), (prevpos) = ((pos)->prev); \
         (pos) != (head); \
         (pos) = (prevpos), (prevpos) = ((pos)->prev))

// 获取外层结构体指针
#define LIST_ENTRY(ptr, type, member) \
    ((type *)((char *)(ptr) - (unsigned long)(&((type *)0)->member)))

#define LIST_IS_EMPTY(head) ((head)->next == (head))

/**
 * 从双向循环链表头部弹出节点（不释放内存）
 * @param head 链表头节点（哨兵节点）
 * @return 弹出的节点指针，如果链表为空返回NULL
 */
#define LIST_POP_HEAD(head) \
    ({ \
        Node *_head = (head); \
        Node *_first = _head->next; \
        (_first == _head) ? NULL : ({ \
            _head->next = _first->next; \
            _first->next->prev = _head; \
            _first->next = _first; \
            _first->prev = _first; \
            _first; \
        }); \
    })

/**
 * 从双向循环链表尾部弹出节点（不释放内存）
 * @param head 链表头节点（哨兵节点）
 * @return 弹出的节点指针，如果链表为空返回NULL
 */
#define LIST_POP_TAIL(head) \
    ({ \
        Node *_head = (head); \
        Node *_last = _head->prev; \
        (_last == _head) ? NULL : ({ \
            _head->prev = _last->prev; \
            _last->prev->next = _head; \
            _last->next = _last; \
            _last->prev = _last; \
            _last; \
        }); \
    })

/**
 * 查看双向循环链表尾部节点（不修改链表）
 * @param head 链表头节点（哨兵节点）
 * @return 尾部节点指针，如果链表为空返回NULL
 */
#define LIST_PEEK_TAIL(head) \
    ({ \
        Node *_head = (head); \
        Node *_last = _head->prev; \
        (_last == _head) ? NULL : _last; \
    })

/**
 * 将源链表的全部节点转移到目标链表尾部
 * @param dest_head 目标链表头节点
 * @param src_head 源链表头节点
 * 注意：操作后源链表将为空
 */
#define LIST_TRANSFER_ALL_TAIL(dest_head, src_head) \
    do { \
        if (!LIST_IS_EMPTY(src_head)) { \
            Node *_dest = (dest_head); \
            Node *_src = (src_head); \
            Node *_src_first = _src->next; \
            Node *_src_last = _src->prev; \
            \
            /* 断开源链表 */ \
            _src->next = _src; \
            _src->prev = _src; \
            \
            /* 连接到目标链表尾部 */ \
            _src_first->prev = _dest->prev; \
            _src_last->next = _dest; \
            _dest->prev->next = _src_first; \
            _dest->prev = _src_last; \
        } \
    } while (0)

#ifdef __cplusplus
}
#endif
#endif /* LINKEDLIST_H */
