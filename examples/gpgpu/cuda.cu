//:: cases BasicCuda
//:: verdict Pass

#include <cuda.h>

/*@
    context \pointer_index(a, threadIdx.x, write);
/@*/
__global__ void example(int a[], int len) {
    int tid = threadIdx.x;
    a[tid] = tid;
    /*@
        context \pointer_index(a, threadIdx.x, write);
    @*/
    __syncthreads();
    a[tid] = a[tid] * 2;
}