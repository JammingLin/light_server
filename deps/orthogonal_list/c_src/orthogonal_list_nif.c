#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "erl_nif.h"
#include "orthogonal_list.h"

static ERL_NIF_TERM create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM release(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM get_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM print_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM foreach(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

//nif define
static ErlNifFunc nif_funcs[] =
{
    {"create",      2, create},
    {"release",     1, release},
    {"get_value",   3, get_value},
    {"update",      4, update},
    {"print_list", 1, print_list},
    {"foreach",     2, foreach}
};

static ERL_NIF_TERM create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 2) return enif_make_badarg(env);

    int rows, cols;
    if(!enif_get_int(env, argv[0], &rows)) return enif_make_badarg(env);
    if(!enif_get_int(env, argv[1], &cols)) return enif_make_badarg(env);

    struct CrossList *cl = ol_create(rows, cols);
    if(NULL == cl) return enif_make_atom(env, "create_fail");

    printf("crosslist rows %d cols %d count %d rhead %p chead %p \n addr %lu, %p\n",
    cl->rows, cl->cols, cl->count, cl->Rhead, cl->Chead, (unsigned long)cl, cl);

    return enif_make_ulong(env, (unsigned long)cl);
}

static ERL_NIF_TERM release(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 1) return enif_make_badarg(env);

    unsigned long objPtr;
    if(!enif_get_ulong(env, argv[0], &objPtr))
      return enif_make_badarg(env);

    struct CrossList *cl = (struct CrossList*)objPtr;
    if(NULL == cl) return enif_make_atom(env, "object_is_null");
    ol_release(cl);
    printf("release %p\n", cl);
    return enif_make_atom(env, "release_ok");
}

static ERL_NIF_TERM get_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 3) return enif_make_badarg(env);

    unsigned long objPtr;
    if(!enif_get_ulong(env, argv[0], &objPtr))
        return enif_make_badarg(env);

    struct CrossList *cl = (struct CrossList*)objPtr;
    if(NULL == cl) return enif_make_atom(env, "object_is_null");
    //if(cl->count==0) return enif_make_atom(env, "list_is_empty");
    printf("cl addr %p, rows %d, cols %d, count %d rhead %p chead %p\n",
    cl, cl->rows, cl->cols, cl->count, cl->Rhead, cl->Chead);

    int row, col;
    if(!enif_get_int(env, argv[1], &row)) return enif_make_badarg(env);
    if(!enif_get_int(env, argv[2], &col)) return enif_make_badarg(env);

    ElemType et = ol_get(cl, row, col);
    printf("get value eleptr %p elem %d\n", &et, *(int*)et);
    return enif_make_long(env, *(int*)et);
}

static ERL_NIF_TERM update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 4) return enif_make_badarg(env);

    unsigned long objPtr;
    if(!enif_get_ulong(env, argv[0], &objPtr)) return enif_make_badarg(env);
    struct CrossList *cl = (struct CrossList*)objPtr;
    if(NULL == cl) return enif_make_atom(env, "object_is_null");

    int row, col;
    if(!enif_get_int(env, argv[1], &row)) return enif_make_badarg(env);
    if(!enif_get_int(env, argv[2], &col)) return enif_make_badarg(env);

    int value;
    if(!enif_get_int(env, argv[3], &value)) return enif_make_badarg(env);
    ElemType et = (ElemType)&value;
    printf("update elem ptr %p value %d\n", et, *(int*)et),
    ol_update(cl, row, col, et);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM print_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 1) return enif_make_badarg(env);

    unsigned long objPtr;
    if(!enif_get_ulong(env, argv[0], &objPtr))
        return enif_make_badarg(env);

    struct CrossList *cl = (struct CrossList*)objPtr;
    if(NULL == cl) return enif_make_atom(env, "object_is_null");
    ol_print(cl);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM foreach(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 2) return enif_make_badarg(env);

    unsigned long objPtr;
    if(!enif_get_ulong(env, argv[0], &objPtr))
        return enif_make_badarg(env);

    struct CrossList *cl = (struct CrossList*)objPtr;
    if(NULL == cl) return enif_make_atom(env, "object_is_null");

    return enif_make_atom(env, "ok");
}

ERL_NIF_INIT(orthogonal_list_nif, nif_funcs, NULL, NULL, NULL, NULL);
