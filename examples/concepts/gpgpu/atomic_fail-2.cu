//:: cases BasicCuda
//:: tool silicon
//:: verdict Fail

#include <cuda.h>

//missing kernel_invariant for atomic operation

/*@
    context_everywhere blockDim.x > 0 && blockDim.y == 1 && blockDim.z == 1;
    context_everywhere gridDim.x == 1 && gridDim.y == 1 && gridDim.z == 1;    
    context_everywhere in != NULL && out != NULL;
    context_everywhere blockDim.x <= len;
    context_everywhere \pointer_length(in) >= len;
    context_everywhere \pointer_length(out) >= 1;
    context_everywhere (\forall* int i; 0<=i && i < len ; Perm({:in[i]:}, read));
@*/
__global__ void example(int in[], int len, int* out) {
    int tid = threadIdx.x;
    int tmp = in[tid];
    atomicAdd(out, tmp);
}

