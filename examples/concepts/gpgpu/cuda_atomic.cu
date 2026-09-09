//:: cases BasicCuda
//:: tool silicon
//:: verdict Pass

#include <cuda.h>

/*@
 ghost
 requires |in| >= len;
 requires |contrib| >= len;
 requires 0 <= i && i <= len;
 decreases len-i;
pure int sumContribH(seq<int> in, seq<int> contrib, int len, int i){
  if(i==len)
    return 0;
  else {
    int added = contrib[i] == 1 ? in[i] : 0;
    return added + sumContribH(in, contrib, len, i+1);
  }
}

 ghost
 requires |in| >= len;
 requires |contrib| >= len;
 requires len >= 0;
 decreases;
pure int sumContrib(seq<int> in, seq<int> contrib, int len){
  return sumContribH(in, contrib, len, 0);
}

  decreases |in|;
pure int sum(seq<int> in) = |in| == 0 ? 0 : in.head + sum(in.tail);


 ghost 
 requires |in| >= len;
 requires |contrib| >= len;
 requires 0 <= i && i <= len;
 requires 0 <= j && j < len;
 requires contrib[j] == 0;
 ensures j<i ==> sumContribH(in, contrib[j->1], len, i) == sumContribH(in, contrib, len, i);
 ensures j>=i ==> sumContribH(in, contrib[j->1], len, i) == sumContribH(in, contrib, len, i) + in[j];
 decreases len-i;
pure void sumContribEqual(seq<int> in, seq<int> contrib, int len, int i, int j){
  if(i<len){
    return sumContribEqual(in, contrib, len, i+1, j);
  } else {
    return;
  }
}

 ghost
 requires |inp| == len;
 requires |contrib| >= len;
 requires 0 <= i && i <= len;
 requires (\forall int j; 0 <= j && j < len; {:contrib[j]:} == 1);
 ensures sumContribH(inp, contrib, len, len-i) == sum(inp[len-i .. ]);
 decreases i;
pure void sumEqual(seq<int> inp, seq<int> contrib, int len, int i){
  if (i>0) {
    void x = sumEqual(inp, contrib, len, i-1);
    assert inp[len-(i-1) ..] == inp[len-i .. ].tail;
    assert sumContribH(inp, contrib, len, len-i) == sum(inp[len-i .. ]);
  }
  return;
}
  

 ghost 
 requires |in| >= len;
 requires |contrib| >= len;
 requires 0 <= i && i <= len;
 requires (\forall int i; 0 <= i && i < len; {:contrib[i]:} == 0);
 ensures  sumContribH(in, contrib, len, i) == 0;
 decreases len-i;
pure void sumZero(seq<int> in, seq<int> contrib, int len, int i){
  if(i<len){
    return sumZero(in, contrib, len, i+1);
  } else {
    return;
  }
}

  requires arr != NULL && \pointer_length(arr) >= len;
  requires (\forall* int i; 0<=i && i < len ; Perm({:arr[i]:}, read));
  ensures |\result| == len;
  ensures (\forall int i; 0<=i && i < len; {:\result[i]:} == arr[i]);
opaque pure seq<int> toSeq(int* arr, int len);
*/

/*@
    given int* contrib;
    given seq<int> in_seq;
    context blockDim.x == 32 && blockDim.y == 1 && blockDim.z == 1;
    context gridDim.x > 0 && gridDim.y == 1 && gridDim.z == 1;    
    context_everywhere contrib != NULL && \pointer_length(contrib) >= len;
    context_everywhere in != NULL && \pointer_length(in) >= len;
    context_everywhere out != NULL && \pointer_length(out) >= 1;
    context (\forall* int i; 0<=i && i < len ; Perm({:in[i]:}, 1\2\blockDim.x\gridDim.x));
    requires \gtid < len ==> Perm({:contrib[\gtid]:}, 1\2);
    ensures \gtid < len ==> Perm({:contrib[\gtid]:}, read);
    requires \gtid < len ==> {:contrib[\gtid]:} == 0;
    ensures \gtid < len ==> {:contrib[\gtid]:} == 1;
    requires in_seq == toSeq(in, len);
    kernel_invariant Perm(out[0], write) ** 
        |in_seq| == len **
        (\forall* int i; 0<=i && i < len ; Perm({:contrib[i]:}, 1\2)) **
        sumContrib(in_seq, toSeq(contrib, len), len) == out[0]
        ;
@*/
__global__ void sum_kernel(int* in, int len, int* out) {
    int tid = threadIdx.x + blockIdx.x * blockDim.x;
    if (tid < len){
      int tmp = in[tid];
      atomicAdd(out, tmp /*@ then { 
        seq<int> old_contrib = toSeq(contrib, len);
        sumContribEqual(in_seq, old_contrib, len, 0, tid);
        contrib[tid]=1;
        assert toSeq(contrib, len) == old_contrib[tid->1];
      } @*/ ) ;
    }
}

/*@ ensures \pointer(\result, N, write);
    ensures \pointer_length(\result) == N; @*/
int *vercorsCudaMallocInt(int N);
void vercorsCudaFreeInt(int *addr);

/*@ context \pointer(xs, size, write);
    ensures (\forall int i; 0 <= i && i < size; {:xs[i]:} == value); @*/
void setArray(int* xs, int size, int value);

/*@ requires 0 <= cudaMemcpyKind && cudaMemcpyKind <= 4;
    context \pointer(dst, count, write);
    context \pointer(src, count, 1\2);
    ensures (\forall int i; 0<=i && i<count; {:dst[i]:} == {:2:src[i]:});@*/
void vercorsCudaMemcpy(int* dst, int* src, int count, int cudaMemcpyKind);

  /*@ context \pointer(in, size, 1\2);
      ensures \result == sum(toSeq(in, size)); @*/
int sum(int *in, int size) {
  int* d_in = vercorsCudaMallocInt(size);
  int* d_out = vercorsCudaMallocInt(1);
  
  int result[] = {0};
  vercorsCudaMemcpy(d_in, in, size, cudaMemcpyHostToDevice);
  vercorsCudaMemcpy(d_out, result, 1, cudaMemcpyHostToDevice);
  /*@ 
  ghost int* contrib = vercorsCudaMallocInt(size);
  ghost seq<int> in_seq = toSeq(in, size);
  ghost setArray(contrib, size, 0);
  ghost sumZero(in_seq, toSeq(contrib, size), size, 0); 
  */
  sum_kernel<<<(size+32-1)/32,32>>>(d_in, size, d_out) /*@ given {contrib=contrib, in_seq = in_seq}*/;
  //@ ghost sumEqual(in_seq, toSeq(contrib, size), size, size);

  vercorsCudaMemcpy(result, d_out, 1, cudaMemcpyDeviceToHost);
  vercorsCudaFreeInt(d_in);
  vercorsCudaFreeInt(d_out);  
  return result[0];
}
