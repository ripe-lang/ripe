#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define PANIC_RED "\033[1;31m"
#define PANIC_RESET "\033[0m"

static const char *panic_prefix(void) {
    return isatty(fileno(stderr)) ? PANIC_RED "panic:" PANIC_RESET : "panic:";
}

void ripe_panic(const char *msg) {
    fprintf(stderr, "%s %s\n", panic_prefix(), msg);
    abort();
}

void ripe_panic_bounds(long idx, long len) {
    fprintf(stderr, "%s index %ld is out of range for length %ld\n",
            panic_prefix(), idx, len);
    abort();
}

void ripe_panic_slice_bounds(long lo, long hi, long len) {
    fprintf(stderr, "%s slice bounds [%ld:%ld] out of range for length %ld\n",
            panic_prefix(), lo, hi, len);
    abort();
}

void ripe_panic_divzero(void) {
    fprintf(stderr, "%s integer divide by zero\n", panic_prefix());
    abort();
}

void ripe_panic_null(void) {
    fprintf(stderr, "%s null pointer dereference\n", panic_prefix());
    abort();
}
