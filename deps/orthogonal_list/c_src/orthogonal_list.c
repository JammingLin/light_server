#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "orthogonal_list.h"

struct CrossList * ol_create(int rows, int cols)
{
     assert(rows > 0);
     assert(cols > 0);
     struct CrossList *cl = (struct CrossList *)malloc(sizeof(struct CrossList));

     cl->rows = rows;
     cl->cols = cols;
     cl->count = 0;


     cl->Rhead = (OLink *)calloc(rows, sizeof(OLink));
     if(!cl->Rhead)
        exit(-1);


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
