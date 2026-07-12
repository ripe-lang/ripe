#include <stdio.h>
#include <stdlib.h>

void ripe_panic(const char *msg) {
    fprintf(stderr, "panic: %s\n", msg);
    abort();
}

void ripe_panic_bounds(long idx, long len) {
    fprintf(stderr, "panic: index %ld is out of range for length %ld\n", idx, len);
    abort();
}
