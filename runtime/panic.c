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

void ripe_panic_bounds(unsigned long idx, unsigned long len) {
    fprintf(stderr, "%s index %lu is out of range for length %lu\n",
            panic_prefix(), idx, len);
    abort();
}

void ripe_panic_slice_bounds(unsigned long lo, unsigned long hi, unsigned long len) {
    fprintf(stderr, "%s slice bounds [%lu:%lu] out of range for length %lu\n",
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

void ripe_panic_shift(void) {
    fprintf(stderr, "%s negative shift amount\n", panic_prefix());
    abort();
}

void ripe_panic_cast(void) {
    fprintf(stderr, "%s value does not fit in the target type\n", panic_prefix());
    abort();
}
