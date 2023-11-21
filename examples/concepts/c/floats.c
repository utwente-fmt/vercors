#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

int test(int x){
    return x;
}

float test2(float x){
    return x;
}

//@ requires y != 0;
//@ ensures \result == x % y;
int test3(int x, int y){
    return x % y;
}

//@ ensures \result == (x+0.0000001 >= y && x-0.0000001 <= y);
/*@ pure @*/ bool estimate_eq(double x, double y){
    return x+0.0000001 >= y && x-0.0000001 <= y;
}

int main(){
    test(2.0);
    test2(1);
    assert(1.0 == test3(3.0, 2.0));

    int a,b,c,d,e;
    float w,x,y,z;
    int f = 5*2;
    a = 1;
    x = 1.0;
    y = 1.1;
    assert(1!=0);
    assert(y>=x);
    assert(y>x);
    assert(x<y);
    assert(x<=y);
    assert(x!=y);
    assert(!(x==y));

    assert(1.5 != 1);

    assert(y>=a);
    assert(y>a);
    assert(a<y);
    assert(a<=y);
    assert(a!=y);
    assert(!(a==y));

    x = 3.0;
    int bb = 2.0;
    a = x/bb;

    a = 3/2.0;
    assert(a==1);
    a = 3.0/2;
    assert(a==1);
    a = 3.0/2.0;
    assert(a==1);
    x = 1;
    assert(x == 3/2);
    assert(1.5 != 3/2);
    assert(1.5 == 3.0/2.0);
    assert(1.5 == 3.0/2.0);
    assert(1 != 3.0/2.0);
    assert(1 != 3.0/2);
    assert(1 != 3/2.0);

    assert(3 != (1.9*2));
    assert(3.8 == 1.9*2);

    assert(3 != 1.9+2);
    assert(3.9 == 1.9+2);
    assert(0 != 2.9-2);
    assert(estimate_eq(0.9, 1.9-1));

    assert(1 == 3 % 2);

    int sum = 22, count = 5;
    double mean = (double)sum / count;
    assert(mean == 4.4);
    {
        int a,b = 2;
        float x = 9.5;
        double y = 10.5;
        long int z = 50;
        double d;
        a = z/b+b*x-y;
        assert(a == 33);
        d = z/b+b*x-y;
        assert(d == 33.5);
    }
}