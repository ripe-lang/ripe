#include <stdio.h>
#include <stdlib.h>

void ripe_panic(const char *msg) {
    fprintf(stderr, "panic: %s\n", msg);
    abort();
}
