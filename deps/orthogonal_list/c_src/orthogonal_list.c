#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "erl_nif.h"
#include "orthogonal_list.h"

ErlNifResourceType* RES_TYPE;
ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM get_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

//nif define
static ErlNifFunc nif_funcs[] =
{
    {"create", 2, create},
    {"get_value", 3, get_value}
};

typedef struct OLNode {
     int  row, col;          //行号与列号
     ElemType value;        //值
     struct OLNode *right, *down;  //指针域
}OLNode, *OLink;

struct  CrossList{
    OLink   *Rhead, *Chead;
    int rows, cols, count;       // 稀疏矩阵的行数、列数和非零元个数
};

void reversestr(char *source,char target[],unsigned int length)
{
    int i;
    for(i=0;i<length;i++)
    target[i]=source[length-1-i];
    target[i]=0;
}

void tohex(unsigned long num,char *hexStr)
{
    unsigned long n = num;
    char hextable[]="0123456789abcdef";
    char temphex[12],hex[12];
    int i=0;
    while(n)
    {
        temphex[i++]=hextable[n%16];
        n /= 16;
    }
    temphex[i]=0;
    reversestr(temphex,hex,i);
    strcpy(hexStr,hex);
}

static ERL_NIF_TERM create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int rows, cols;
    if(!enif_get_int(env, argv[0], &rows)) return enif_make_badarg(env);
    if(!enif_get_int(env, argv[1], &cols)) return enif_make_badarg(env);

    struct CrossList *cl =  (struct CrossList*)enif_alloc(sizeof(struct CrossList));
    cl = ol_create(rows, cols);
    printf("crosslist rows %d cols %d count %d rhead %p chead %p \n addr %lu, %p\n",
    cl->rows, cl->cols, cl->count, cl->Rhead, cl->Chead, (unsigned long)cl, cl);

    return enif_make_ulong(env, (unsigned long)cl);
}

static ERL_NIF_TERM get_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 3) return enif_make_badarg(env);

    unsigned long addr;
    //struct CrossList *cl = (struct CrossList*)enif_alloc(sizeof(struct CrossList));
    if(!enif_get_ulong(env, argv[0], &addr))
        return enif_make_badarg(env);
    printf("get addr %lu \n", addr);
    char table[12] = {0};
    tohex(addr, table);
    printf("addr %s \n", table);
    struct CrossList *cl = NULL;
    printf("cl addr %p\n", cl);
    
    return enif_make_ulong(env, 100);


//    int rows, cols;
//    if(!enif_get_int(env, argv[1], &rows)) return enif_make_badarg(env);
//    if(!enif_get_int(env, argv[2], &cols)) return enif_make_badarg(env);
//
//    ElemType *et = (ElemType*)enif_alloc(sizeof(ElemType));
//    *et = ol_get(cl, rows, cols);
//
//    enif_release_resource(et);
//    enif_release_resource(cl);

//    enif_free(cl);
//    enif_free(et);

//    return enif_make_resource(env, et);
}



struct CrossList * ol_create(int rows, int cols)
{
     assert(rows > 0);
     assert(cols > 0);
     struct CrossList *cl = (struct CrossList *)malloc(sizeof(struct CrossList));

     cl->rows = rows;
     cl->cols = cols;
     cl->count = 0;

     //创建行链表头数组
     cl->Rhead = (OLink *)calloc(rows, sizeof(OLink));
     if(!cl->Rhead)
        exit(-1);

     //创建列链表头数组
     cl->Chead = (OLink *)calloc(cols,  sizeof(OLink));
     if(!(cl->Chead))
        exit(-1);

    return cl;
}

OLNode* NewNode(int row, int col, ElemType value) {
    OLNode *node = (OLNode *)malloc(sizeof(OLNode));
    node->row = row;
    node->col = col;
    node->value = value;
    node->right = NULL;
    node->down = NULL;
    return node;
}

OLNode* FindNode(struct CrossList *cl, int row, int col) {
    assert(cl != NULL);
    assert(row >= 0 && row < cl->rows);
    assert(col >= 0 && col < cl->cols);

    OLNode *p;
    for (p = cl->Chead[col]; p && p->row <= row; p = p->down){
            if (p->row == row)
                return p;
    }

    if (p)
        return p->down;
    return NULL;
}

ElemType ol_get(struct CrossList *cl, int row, int col) {
    OLNode *node = FindNode(cl, row, col);
    if (node == NULL)
        return NULL;
    return node->value;
}

void ol_update(struct CrossList *cl, int row, int col,  ElemType value){
    assert(cl != NULL);
    assert(row >= 0 && row < cl->rows);
    assert(col >= 0 && col < cl->cols);

    OLNode *node, *p ;
    if (cl->Chead[col] == NULL){
        node = NewNode(row, col, value);
        cl->Chead[col] = node;
    } else {
        for (p = cl->Chead[col]; p->down && p->down->row <= row ; p = p->down)
            ;
        if (p->row == row) {
            printf("p->row == row(%d)\n", row);
            p->value = value;
            node = p;
        } else {
            node = NewNode(row, col, value);
            if (row < p->row) {
                node->down = p;
                cl->Chead[col] = node;
            } else {
                node->down = p->down;
                p->down = node;
            }
        }
    }

    if (cl->Rhead[row] == NULL){
        assert(node != NULL);
        cl->Rhead[row] = node;
    } else {
        for (p = cl->Rhead[row]; p->right && p->right->col <= col ; p = p->right)
            ;
        if (col < p->col){
            node->right = p;
            cl->Rhead[row] = node;
        } else {
            node->right = p->right;
            p->right = node;
        }
    }
}


void ol_foreach(struct CrossList *cl, foreach_callback cb) {
    for (int col = 0; col < cl->cols; col++){
        for (OLNode *p = cl->Chead[col]; p; p = p->down){
            cb(p->row, p->col, p->value);
        }
    }
}

void ol_release(struct CrossList *cl) {
    for (int col = 0; col < cl->cols; col++){
        for (OLNode *p = cl->Chead[col]; p; ){
            printf("%d, ", *(int*)p->value);
            OLNode *down = p->down;
            free(p);
            p = down;
        }
    }
    free(cl->Rhead);
    free(cl->Chead);
}

void ol_print(struct CrossList *cl) {
    for (int row = 0; row < cl->rows; row++){
        OLNode *p = cl->Rhead[row];
        for (int col = 0; col < cl->cols; col++){
            if (p && p->col == col){
                printf("%4d", *(int*)p->value);
                p = p->right;
            } else {
                printf("%4d", 0);
            }
        }
        printf("\n");
    }
}

ERL_NIF_INIT(orthogonal_list, nif_funcs, NULL, NULL, NULL, NULL);
