#ifndef _ORTHOGONAL_LIST_H_
#define _ORTHOGONAL_LIST_H_

//c

struct CrossList; 
typedef  void* ElemType;

typedef void (foreach_callback)(int row, int col, ElemType value);

// 创建和销毁稀疏矩阵 
struct CrossList * ol_create(int rows, int cols) ;
void ol_release(struct CrossList *cl);

// 获得值
ElemType ol_get(struct CrossList *cl, int row, int col);

// 更新或者加入值
void ol_update(struct CrossList *cl, int row, int col,  ElemType value);

void ol_print(struct CrossList *cl);

void ol_foreach(struct CrossList *cl, foreach_callback cb);

#endif