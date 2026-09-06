// SPDX-License-Identifier: Apache-2.0
typedef struct {
    int x;
    int y;
} Point;

typedef struct {
    long a;
    long b;
} Wide;

typedef struct {
    long a;
    long b;
    long c;
} Big;

typedef struct {
    double x;
    double y;
} Pair;

typedef struct {
    int n;
    double f;
} Mixed;

Point c_shift(Point p, int d) {
    Point r = {p.x + d, p.y + d};
    return r;
}

Wide c_widen(Wide w) {
    Wide r = {w.a * 2, w.b * 2};
    return r;
}

Big c_grow(Big b, long d) {
    Big r = {b.a + d, b.b + d, b.c + d};
    return r;
}

Pair c_scale(Pair p, double k) {
    Pair r = {p.x * k, p.y * k};
    return r;
}

Mixed c_blend(Mixed m) {
    Mixed r = {m.n + 1, m.f + 0.5};
    return r;
}

void c_bump(Wide *w) {
    w->a += 1;
    w->b += 1;
}
