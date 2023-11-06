#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

struct d{
 int x;
};

int main(){
    int* xs = (int*) malloc(sizeof(int)*3);

    xs[0] = 3;
    xs[1] = 2;
    xs[2] = 1;
    free(xs);

    int** xxs = (int * *) malloc(sizeof(int *)*3);

    int temp[3] = {1,2,3};
    xxs[0] = temp;
    assert(xxs[0][0] == 1);
    free(xxs);

    struct d* ys = (struct d*) malloc(3*sizeof(struct d));
    ys[0].x = 3;
    ys[1].x = 2;
    ys[2].x = 1;
    free(ys);

    float * z = (float *) malloc(sizeof(float));
    z[0] = 3.0;
    *z = 2.0;
    free(z);

    return 0;
}