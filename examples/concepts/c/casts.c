#include <math.h>

static inline float /*@ pure @*/ ceil_f32(float x){ return (float) ceil((double) x); }

int main(){
    float a = 0.510000;
    int b1 = (int)(ceil_f32((a + -0.500000)));
    int b2 = (int) (a + 0.5f);
    int b3 = (int) a + 1;
    float c1 = (float) b1;
    float c2 = (float) b1 + 0.5f;
    float c3 = (float) (b1 + 3);

    //@ assert b1 == 1;
    //@ assert b2 == 1;
    //@ assert b3 == 1;
    //@ assert c1 == 1.0;
    //@ assert c2 == 1.5;
    //@ assert c3 == 4.0;

    return 0;
}