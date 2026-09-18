//:: cases BasicCuda
//:: tool silicon
//:: verdict Pass

#include <cuda.h>

/*@
    context_everywhere a != NULL && \pointer_length(a) >= blockDim.x;
    context blockDim.y == 1 && blockDim.z == 1 && gridDim.x == 1 && gridDim.y == 1 && gridDim.z == 1;
    context Perm({:a[threadIdx.x]:}, write);
@*/
__global__ void example(int a[]) {
    int tid = threadIdx.x;
    a[tid] = tid;
    /*@
        context Perm({:a[threadIdx.x]:}, write);
    @*/
    __syncthreads();
    a[tid] = a[tid] * 2;
}