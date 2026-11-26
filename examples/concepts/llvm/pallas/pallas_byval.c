/**
 * Test that the byval-attribute in LLVM (e.g. used when passing big structs by-value)
 * is encoded correctly in the LLVM-Frontend.
 * 
 * Expected to pass
 */
#include "stdint.h"

/*@
declare DEF_RESULT(int);
@*/

struct {
    int64_t a;
    int64_t b;
    int64_t c;
    int64_t d;
} typedef BigStruct;

/*@
requires _Perm(&s.a, _write);
requires _Perm(&s.b, _write);
requires _Perm(&s.c, _write);
requires _Perm(&s.d, _write);
@*/
void do_a_thing(BigStruct s) {
    s.a = 0;
    s.b = s.a + 1;
}

/*@
ensures _result(int) == 3;
@*/
int a_function() {
    BigStruct s;
    s.a = 1;
    s.b = 2;
    do_a_thing(s);

    /*@
    assert s.a == 1;
    assert s.b == 2;
    @*/
    int sum = s.a + s.b;
    return sum;
}