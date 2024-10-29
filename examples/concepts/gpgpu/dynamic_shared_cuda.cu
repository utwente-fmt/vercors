//:: cases DynamicSharedCuda
//:: tool silicon
//:: verdict Pass

#include <cuda.h>

/*@
  context blockDim.x == 32 && blockDim.y == 1 && blockDim.z == 1;
  context gridDim.x > 0 && gridDim.y == 1 && gridDim.z == 1;

  context in != NULL && out != NULL;
  context \pointer_length(in) == 1;
  context \pointer_length(out) == n;
  context n > 0;
  context blockDim.x * gridDim.x >= n;
  context Perm(&in[0], write \ (blockDim.x * gridDim.x));
  context \gtid<n ==> Perm(&out[\gtid], write);

  context \shared_mem_size(s) == 1;
  requires \ltid == 0 ==> Perm(&s[0], write);

  ensures \gtid<n ==> out[\gtid] == \old(out[\gtid]) + in[0];
@*/
__global__ void blur_x(int* in, int* out, int n) {
  extern __shared__ int s[];
  int tid = blockIdx.x * blockDim.x + threadIdx.x;
  if(threadIdx.x == 0) {
    s[threadIdx.x] = in[0];
  }

  /*@
    context Perm(&in[0], write \ (blockDim.x * gridDim.x));
    context blockIdx.x * blockDim.x + threadIdx.x<n ==> Perm(&out[blockIdx.x * blockDim.x + threadIdx.x], write);
    context blockIdx.x * blockDim.x + threadIdx.x<n ==> \old(out[blockIdx.x * blockDim.x + threadIdx.x]) == out[blockIdx.x * blockDim.x + threadIdx.x];

    requires threadIdx.x == 0 ==> Perm(&s[0], write);
    requires threadIdx.x == 0 ==> s[0] == in[0];

    ensures Perm(&s[0], write \ blockDim.x);

    ensures s[0] == in[0];
  @*/
  __syncthreads();

  if(tid < n) {
    out[tid] += s[0];
  }
}