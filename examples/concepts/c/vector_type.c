#include <stdio.h>
#include <assert.h>

typedef int v4si __attribute__ ((vector_size (sizeof(int)*4)));
typedef float v4sf __attribute__ ((vector_size (sizeof(float)*4)));

/*@
    requires (\forall int i; i>=0 && i<4 ==> 0<={:mask[i]:} && {:mask[i]:}<4);
    ensures \result[0] == x[mask[0]] 
        && \result[1] == x[mask[1]] 
        && \result[2] == x[mask[2]] 
        && \result[3] == x[mask[3]];
@*/
/*@ pure @*/v4si shuffle(v4si x, v4si mask){
    v4si res = {x[mask[0]], x[mask[1]], x[mask[2]], x[mask[3]]};
    return res;
}


int main(){
    int x __attribute__((__vector_size__(sizeof(int)*4))) = {4,3,2,1};
    v4si y = {1,2,3,4};
    x = y;


    assert(x[0] == 1);
    y[1] = 5;
    y = x + y;
    
    assert(y[0] == 2 && y[1] == 7 && y[2] == 6 && y[3] == 8);
    
    y = y * y;
    
    assert(y[0] == 4 && y[1] == 49 && y[2] == 36 && y[3] == 64);
    
    y = y - x;
    
    assert(y[0] == 4-1 && y[1] == 49-2 && y[2] == 36-3 && y[3] == 64-4);

    y = y / x;
    
    assert(y[0] == (4-1)/1 && y[1] == (49-2)/2 && y[2] == (36-3)/3 && y[3] == (64-4)/4);
    
    v4si mask = {0, 1, 1, 3};
    v4si res = shuffle(x, mask);
    
    assert(res[1] == x[1] && res[1] == x[1] && res[2] == x[1] && res[3] == x[3]);
    
    v4sf z = {1,2,3,4};
    float z2 = 5;
    z2 = z2 + z2;
    z = z + z;

    return 0;
}