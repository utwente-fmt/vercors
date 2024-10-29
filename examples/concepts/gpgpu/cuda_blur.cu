//:: cases SharedCuda
//:: tool silicon
//:: verdict Pass

#include <cuda.h>

/*@
  context blockDim.x == 32 && blockDim.y == 1 && blockDim.z == 1;
  context gridDim.x > 0 && gridDim.y == 1 && gridDim.z == 1;
  context in != NULL && out != NULL;
  context \pointer_length(in) == n;
  context \pointer_length(out) == n-2;
  context blockDim.x * gridDim.x >= n;
  context (\forall* int i; 0<=i && i<n; Perm(&in[i], write \ (blockDim.x * gridDim.x)));

  context \gtid<n-2 ==> Perm(&out[\gtid], write);

  context \shared_mem_size(s) == blockDim.x+2;
  requires Perm(&s[\ltid], write);
  requires threadIdx.x < 2 ==> Perm(&s[\ltid + blockDim.x], write);

  ensures \gtid<n-2 ==>out[\gtid] == (in[\gtid] + in[\gtid+1] + in[\gtid+2])/3;
  
@*/
__global__ void blur_x(int* in, int* out, int n) {
  extern __shared__ int s[];
  int tid = blockIdx.x * blockDim.x + threadIdx.x;
  if(tid < n) {
    s[threadIdx.x] = in[tid];
  }
  if(threadIdx.x < 2 && tid+blockDim.x < n){
    s[blockDim.x+threadIdx.x] = in[tid+blockDim.x];
  }

  /*@
    context (\forall* int i; 0<=i && i<n; Perm(&in[i], write \ (blockDim.x * gridDim.x)));
    context blockIdx.x * blockDim.x + threadIdx.x<n-2 ==> Perm(&out[blockIdx.x * blockDim.x + threadIdx.x], write);

    requires Perm(&s[threadIdx.x], write);
    requires threadIdx.x < 2 ==> Perm(&s[threadIdx.x + blockDim.x], write);

    requires blockIdx.x * blockDim.x + threadIdx.x < n
      ==> s[threadIdx.x] == in[blockIdx.x * blockDim.x + threadIdx.x];
    requires threadIdx.x < 2 && blockIdx.x * blockDim.x + threadIdx.x+blockDim.x < n
      ==> s[threadIdx.x+blockDim.x] == in[blockIdx.x * blockDim.x + threadIdx.x+blockDim.x];


    ensures (\forall* int i; 0 <= i && i < blockDim.x+2; Perm(&s[i], write \ (blockDim.x+2)));

    ensures 
      (\forall int i; 0 <= i && i < blockDim.x+2 && blockIdx.x*blockDim.x + i <n
        ==> s[i] == in[blockIdx.x * blockDim.x+i]);
  @*/
  __syncthreads();

  if(tid < n-2) {
    out[tid] = (s[threadIdx.x] + s[threadIdx.x+1] + s[threadIdx.x+2])/3;
  }
}