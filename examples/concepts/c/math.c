#include <assert.h>
#include <math.h>

int main(){
    //@ assert(ceil(1.5) == 2.0);
    //@ assert(ceil(2.0) == 2.0);
    //@ assert(ceil(-1.5) == -1.0);

    //@ assert((int)ceil(1.5) == 2);
    //@ assert((int)ceil(2.0) == 2);
    //@ assert((int)ceil(-1.5) == -1);

    //@ assert(floor(1.5) == 1.0);
    //@ assert(floor(2.0) == 2.0);
    //@ assert(floor(-1.5) == -2.0);

    //@ assert((int)floor(1.5) == 1);
    //@ assert((int)floor(2.0) == 2);
    //@ assert((int)floor(-1.5) == -2);

    //@ assert(round(1.5) == 2.0);
    //@ assert(round(1.4) == 1.0);
    //@ assert(round(2.0) == 2.0);
    //@ assert(round(-1.5) == -2.0);
    //@ assert(round(-1.4) == -1.0);

    //@ assert((int)round(1.5) == 2);
    //@ assert((int)round(1.4) == 1);
    //@ assert((int)round(2.0) == 2);
    //@ assert((int)round(-1.5) == -2);
    //@ assert((int)round(-1.4) == -1);

    //@ assert(fabs(-1.5) == 1.5);
    //@ assert(fabs(1.5) == 1.5);

    //@ assert(pow(2.0, 2.0) == 4.0);
    //@ assert(pow(2.0, 0.0) == 1.0);
    //@ assert(pow(4.0, 0.5) == 2.0);

    //@ assert(sqrt(4.0) == 2.0);
}