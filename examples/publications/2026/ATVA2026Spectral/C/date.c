// Transform with mem2reg-option
#include <stdbool.h>

/*@
declare DEF_RESULT(_Bool);
@*/

/*@
requires _and(1 <= m1, m1 <= 12);
requires _and(1 <= d1, d1 <= 31);
requires _and(1 <= m2, m2 <= 12);
requires _and(1 <= d2, d2 <= 31);
ensures _imply(y1 > y2,
              _result(_Bool) == true);
ensures _imply(_and(y1 == y2, m1 == m2),
               _result(_Bool) == d1 > d2);
@*/
bool later(int y1, int m1, int d1,
           int y2, int m2, int d2) {
    if (y1 != y2) {
        return y1 > y2;
    } else if (m1 != m2) {
        return m1 > m2;
    } else {
        return d1 > d2;
    }
}

int test() {
    later(2023, 03, 07,
          2023, 01, 01);
    later(01, 01, 2023,
          15, 03, 2023);
    return 0;
}