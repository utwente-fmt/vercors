//:: cases AtomicFail
//:: tool silicon
//:: verdict Fail

#include <cuda.h>


/*@
    context_everywhere blockDim.x > 0 && blockDim.y == 1 && blockDim.z == 1;
    context_everywhere gridDim.x == 1 && gridDim.y == 1 && gridDim.z == 1;    
    context_everywhere in != NULL && out != NULL;
    context_everywhere blockDim.x <= len;
    context_everywhere \pointer_length(in) >= len;
    context_everywhere \pointer_length(out) >= 1;
    context_everywhere (\forall* int i; 0<=i && i < len ; Perm({:in[i]:}, read));
    kernel_invariant Perm(out[0], write);
@*/
__global__ void example(int in[], int len, int* out) {
    int tid = threadIdx.x;
    int tmp = in[tid];
    atomicAdd(out, tmp);
}

/*@ ensures \pointer(\result, N, write);
    ensures \pointer_length(\result) == N; @*/
int *vercorsCudaMallocInt(int N);
void vercorsCudaFreeInt(int *addr);

int main() {
  int size = 100;
  int* d_a = vercorsCudaMallocInt(size);
  int* out = &d_a[0];
  //kernel_invariant not established  
  example<<<1,size>>>(d_a, size, out);

  vercorsCudaFreeInt(d_a);
  vercorsCudaFreeInt(out);  
}
