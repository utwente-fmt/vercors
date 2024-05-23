#include <assert.h>

/*@
  requires y != 0;
  ensures \result == \euclidean_div (x, y);
@*/
/*inline*/ int /*@ pure @*/ div_eucl(int x, int y)
{
    int q = x/y;
    int r = x%y;
    return r < 0 ? q + (y > 0 ? -1 : 1) : q;
}

/*@
  requires y != 0;
  ensures \result == \euclidean_mod(x, y);
@*/
/* inline */ int /*@ pure @*/ mod_eucl(int x, int y)
{
    int r = x%y;
    return r < 0 ? r + (y > 0 ? y : -y) : r;
}

int main(){
    // See Division and Modulus for Computer Scientists by DAAN LEIJEN
    // https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
    // Truncated div and mod
    assert(8/3 == 2);
    assert(8%3 == 2);

    assert(8/-3 == -2);
    assert(8%-3 == 2);

    assert(-8/3 == -2);
    assert(-8%3 == -2);

    assert(-8/-3 == 2);
    assert(-8%-3 == -2);

    assert(1/2 == 0);
    assert(1%2 == 1);

    assert(1/-2 == 0);
    assert(1%-2 == 1);

    assert(-1/2 == 0);
    assert(-1%2 == -1);

    assert(-1/-2 == 0);
    assert(-1%-2 == -1);

    // Euclidean div and mod
    assert(div_eucl(8, 3) == 2);
    //@ assert(\euclidean_div(8, 3) == 2);
    assert(mod_eucl(8, 3) == 2);
    //@ assert(\euclidean_mod(8, 3) == 2);

    assert(div_eucl(8, -3) == -2);
    //@ assert(\euclidean_div(8, -3) == -2);
    assert(mod_eucl(8, -3) == 2);
    //@ assert(\euclidean_mod(8, -3) == 2);

    assert(div_eucl(-8,3) == -3);
    //@ assert(\euclidean_div(-8,3) == -3);
    assert(mod_eucl(-8, 3) == 1);
    //@ assert(\euclidean_mod(-8, 3) == 1);

    assert(div_eucl(-8,-3) == 3);
    //@ assert(\euclidean_div(-8,-3) == 3);
    assert(mod_eucl(-8, -3) == 1);
    //@ assert(\euclidean_mod(-8, -3) == 1);

    assert(div_eucl(1, 2) == 0);
    //@ assert(\euclidean_div(1, 2) == 0);
    assert(mod_eucl(1, 2) == 1);
    //@ assert(\euclidean_mod(1, 2) == 1);

    assert(div_eucl(1, -2) == 0);
    //@ assert(\euclidean_div(1, -2) == 0);
    assert(mod_eucl(1, -2) == 1);
    //@ assert(\euclidean_mod(1, -2) == 1);

    assert(div_eucl(-1,2) == -1);
    //@ assert(\euclidean_div(-1,2) == -1);
    assert(mod_eucl(-1, 2) == 1);
    //@ assert(\euclidean_mod(-1, 2) == 1);

    assert(div_eucl(-1,-2) == 1);
    //@ assert(\euclidean_div(-1,-2) == 1);
    assert(mod_eucl(-1, -2) == 1);
    //@ assert(\euclidean_mod(-1, -2) == 1);

    return 0;
}