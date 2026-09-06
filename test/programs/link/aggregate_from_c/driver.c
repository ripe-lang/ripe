// SPDX-License-Identifier: Apache-2.0
#include <stdio.h>

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
    float x;
    float y;
} Vec2;

typedef struct {
    int n;
    double f;
} Mixed;

typedef struct {
    int v[4];
} Held;

Point shift(Point p, int d);
Wide widen(Wide w);
Big grow(Big b, long d);
Vec2 scale(Vec2 v, float k);
Mixed blend(Mixed m);
int total(Held h);
void bump(Wide *w);

int main(void) {
    Point p = shift((Point){10, 20}, 5);
    printf("point %d %d\n", p.x, p.y);

    Wide w = widen((Wide){100, 200});
    printf("wide %ld %ld\n", w.a, w.b);

    Big b = grow((Big){1, 2, 3}, 10);
    printf("big %ld %ld %ld\n", b.a, b.b, b.c);

    Vec2 v = scale((Vec2){1.5f, 2.5f}, 4.0f);
    printf("vec2 %g %g\n", v.x, v.y);

    Mixed m = blend((Mixed){7, 0.25});
    printf("mixed %d %g\n", m.n, m.f);

    printf("total %d\n", total((Held){{1, 2, 3, 4}}));

    Wide own = {41, 51};
    bump(&own);
    printf("bump %ld %ld\n", own.a, own.b);
    return 0;
}
