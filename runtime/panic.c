#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define PANIC_RED "\033[1;31m"
#define PANIC_RESET "\033[0m"
#define RIPE_PANIC_STATUS 79

struct ripe_site {
    unsigned int file;
    unsigned int line;
    unsigned int col;
    unsigned int func;
};

/* Clang lets these stay undefined so a program with no checks links without
   emitting either table */
extern const struct ripe_site ripe_panic_sites[] __attribute__((weak));
extern const char ripe_panic_strtab[] __attribute__((weak));

static int use_color(void) {
    const char *no_color = getenv("NO_COLOR");

    if (no_color != NULL && no_color[0] != '\0') {
        return 0;
    }
    return isatty(fileno(stderr));
}

static const char *panic_prefix(void) {
    return use_color() ? PANIC_RED "panic:" PANIC_RESET : "panic:";
}

static _Noreturn void report(unsigned int site, const char *fmt, ...) {
    const struct ripe_site *s = &ripe_panic_sites[site];
    va_list args;

    fprintf(stderr, "%s ", panic_prefix());
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fputc('\n', stderr);

    fprintf(stderr, "  at %s:%u:%u\n", &ripe_panic_strtab[s->file], s->line,
            s->col);
    fprintf(stderr, "  in %s\n", &ripe_panic_strtab[s->func]);

    /* A status clear of the sysexits.h range and of the 128+N a shell reports
       for a signal keeps a panic from looking like an ordinary failure */
    exit(RIPE_PANIC_STATUS);
}

_Noreturn void ripe_panic(unsigned int site, const char *msg) {
    report(site, "%s", msg);
}

_Noreturn void ripe_panic_bounds(unsigned int site, unsigned long idx,
                                 unsigned long len) {
    report(site, "index %lu is out of range for length %lu", idx, len);
}

_Noreturn void ripe_panic_slice_bounds(unsigned int site, unsigned long lo,
                                       unsigned long hi, unsigned long len) {
    report(site, "slice bounds [%lu:%lu] out of range for length %lu", lo, hi,
           len);
}

_Noreturn void ripe_panic_divzero(unsigned int site) {
    report(site, "integer divide by zero");
}

_Noreturn void ripe_panic_null(unsigned int site) {
    report(site, "null pointer dereference");
}

_Noreturn void ripe_panic_shift(unsigned int site) {
    report(site, "negative shift amount");
}
