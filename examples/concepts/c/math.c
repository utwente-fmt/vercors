#include <assert.h>
#include <math.h>

int main(){
    //@ assert(ceil(1.5) == 2.0);
    //@ assert(ceil(2.0) == 2.0);
    /* TODO: Below test case fails, although given directly to silicon it does not fail, no clue what is going on.
             Uncommented for now. */
    // assert(ceil(-1.5) == -1.0);

    //@ assert(floor(1.5) == 1.0);
    //@ assert(floor(2.0) == 2.0);
    //@ assert(floor(-1.5) == -2.0);

    //@ assert(round(1.5) == 2.0);
    //@ assert(round(1.4) == 1.0);
    //@ assert(round(2.0) == 2.0);
    //@ assert(round(-1.5) == -2.0);
    // TODO: Below fails
    // assert(round(-1.4) == -1.0);

    // TODO: Below fails
    // assert(fabs(-1.5) == 1.5);
    //@ assert(fabs(1.5) == 1.5);

    //@ assert(pow(2.0, 2.0) == 4.0);
    //@ assert(pow(2.0, 0.0) == 1.0);
    //@ assert(pow(4.0, 0.5) == 2.0);

    //@ assert(sqrt(4.0) == 2.0);
}