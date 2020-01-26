#include <string.h>
#include "erl_nif.h"

static ErlNifResourceType* rc4_ctx_rtype = NULL;
struct rc4_ctx_t
{
    unsigned char s[256];
    unsigned char i, j;
};

static void rc4_ctx_dtor(ErlNifEnv *env, struct rc4_ctx_t *ctx)
{

}

static int rc4_load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL, "RC4_CTX", (ErlNifResourceDtor*)rc4_ctx_dtor, ERL_NIF_RT_CREATE, NULL);
    if (rt == NULL)
        return -1;
    rc4_ctx_rtype = rt;
    return 0;
}

static struct rc4_ctx_t *rc4_dup(ErlNifEnv *env, struct rc4_ctx_t *rc4)
{
    struct rc4_ctx_t *new;
    new = enif_alloc_resource(rc4_ctx_rtype, sizeof(*rc4));
    memcpy(new, rc4, sizeof(*rc4));
    return new;
}

static ERL_NIF_TERM rc4_new_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int i;
    unsigned char j, tmp;
    struct rc4_ctx_t *rc4;
    ErlNifBinary data;
    unsigned char *key;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &data))
        return enif_make_badarg(env);
    key = data.data;
    rc4 = enif_alloc_resource(rc4_ctx_rtype, sizeof(*rc4));
    if (rc4 == NULL)
        return enif_make_atom(env,"notsup");
    for (i = 0; i < 256; i++)
    {
        rc4->s[i] = (unsigned char)i;
    }
    j = 0;
    for (i = 0; i < 256; i++)
    {
        j += rc4->s[i] + key[i%data.size];
        tmp = rc4->s[i];
        rc4->s[i] = rc4->s[j];
        rc4->s[j] = tmp;
    }
    rc4->i = 0;
    rc4->j = 0;
    ret = enif_make_resource(env, rc4);
    enif_release_resource(rc4);
    return ret;
}


static ERL_NIF_TERM rc4_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned char i, j;
    unsigned char *src;
    struct rc4_ctx_t *rc4, *new;
    unsigned char *outp;
    ERL_NIF_TERM outb, ret;
    ErlNifBinary data;
    if (!enif_get_resource(env, argv[0], rc4_ctx_rtype, (void**)&rc4))
        return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[1], &data))
        return enif_make_badarg(env);
    new = rc4_dup(env, rc4);
    outp = enif_make_new_binary(env, data.size, &outb);

    i = new->i;
    j = new->j;
    src = (unsigned char*)data.data;
    for (int n = 0; n < data.size; n++)
    {
        unsigned char x,y,tmp;
        i++;
        x = new->s[i];
        j += x;
        y = new->s[j];
        tmp=new->s[i];
        new->s[i]=new->s[j];
        new->s[j]=tmp;
        outp[n]=src[n]^new->s[(unsigned char)(x+y)];
    }
    new->i=i;
    new->j=j;
    ret = enif_make_tuple2(env, enif_make_resource(env, new), outb);
    enif_release_resource(new);
    return ret;
}

static ErlNifFunc rc4_nif_funcs[] = {
    {"new", 1, rc4_new_nif},
    {"update", 2,rc4_update_nif},
};

ERL_NIF_INIT(rc4, rc4_nif_funcs, rc4_load, NULL, NULL, NULL);

