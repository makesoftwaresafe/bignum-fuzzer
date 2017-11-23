#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <bndiff/module.h>
#include <bndiff/operation.h>
#include <bndiff/bignum.h>
#include <chicken.h>

extern void chicken_bignum_initialize(int);
extern int chicken_bignum_from_string(const char *);
extern char *chicken_string_from_bignum(int);
extern int chicken_bignum_operation(int, int, int, int, int, int);
extern void chicken_bignum_cleanup(int);
extern void chicken_bignum_shutdown(void);

static int numbers[NUM_BIGNUMS];

static int initialize(void)
{
    int i;
    for (i = 0; i < NUM_BIGNUMS; i++) {
        numbers[i] = i;
    }

    CHICKEN_initialize(0, 0, 0, C_toplevel);
    CHICKEN_run(C_toplevel);
    chicken_bignum_initialize(NUM_BIGNUMS);
    return 0;
}

static int bignum_from_string(const char* input, void** output)
{
    int position = chicken_bignum_from_string(input);
    *output = (void*)&(numbers[position]);
    return 0;
}

static int string_from_bignum(void* input, char** output)
{
    *output = chicken_string_from_bignum(*(int *)input);
    return 0;
}

static void destroy_bignum(void* bignum)
{
    chicken_bignum_cleanup(*(int *)bignum);
}

static int operation(
        bignum_cluster_t* bignum_cluster,
        operation_t operation,
        uint8_t opt)
{
    int A, B, C, D;
    A = *(int*)bignum_cluster->BN[0],
    B = *(int*)bignum_cluster->BN[1];
    C = *(int*)bignum_cluster->BN[2];
    D = *(int*)bignum_cluster->BN[3];
    return (int)chicken_bignum_operation(operation, A, B, C, D, opt);
}

static void shutdown(void) {
    chicken_bignum_shutdown();
}

module_t mod_chicken = {
    .initialize = initialize,
    .bignum_from_string = bignum_from_string,
    .string_from_bignum = string_from_bignum,
    .destroy_bignum = destroy_bignum,
    .operation = operation,
    .shutdown = shutdown,
    .name = "CHICKEN Scheme"
};
