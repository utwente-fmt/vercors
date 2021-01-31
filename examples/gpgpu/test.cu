/***********************************************************************************
Created by Mohsen Safari.
************************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <cuda.h>

////////////////////////////////////////////////////////////////////////////////
//Kernel
////////////////////////////////////////////////////////////////////////////////

/*@
  yields seq<int> contrib; 
  given seq<int> cost_seq;
  given seq<int> oldcost_seq;
  given seq<int> start_seq;
  given seq<int> end_seq;
  given seq<int> weight_seq; 

  context_everywhere V == 1024 && A == 10 * V;
  context_everywhere opencl_gsize == V;
  context_everywhere 10 == opencl_gcount;
  context_everywhere source >= 0 && source < V;
  context_everywhere counter >= 0 && counter < V-1;
  context_everywhere |start_seq| == A && |end_seq| == A && |weight_seq| == A && |cost_seq| == V && |contrib| == A;
  
  kernel_invariant \pointer(g_start, A, 1\2);
  kernel_invariant \pointer(g_end, A, 1\2);
  kernel_invariant (\forall int i; 0 <= i && i < A; g_start[i] >= 0 && g_start[i] < V && g_end[i] >= 0 && g_end[i] < V);
  kernel_invariant (\forall int i; 0 <= i && i < A; g_start[i] != g_end[i]);
  kernel_invariant (\forall int i; 0 <= i && i < A; (\forall int j; 0 <= j && j < A && i != j; g_start[i] == g_start[j] ==> g_end[i] != g_end[j]));
  
  kernel_invariant \pointer(g_weight, A, 1\2);
  kernel_invariant (\forall int i; 0 <= i && i < A; g_weight[i] > 0);
  
  kernel_invariant \pointer(g_cost, V, write);

  //kernel_invariant (\forall int i; 0 <= i && i < A; g_start[i] == start_seq[i]);
  //kernel_invariant (\forall int i; 0 <= i && i < A; g_end[i] == end_seq[i]);
  //kernel_invariant (\forall int i; 0 <= i && i < A; g_weight[i] == weight_seq[i]); 
   
  

@*/
__global__ void CUDAKernel(int* g_start, int* g_end, int* g_weight, int* g_cost, int V, int A, int counter, int source)
{
  int tid = blockIdx.x * V + threadIdx.x;
  //@ assert tid == \gtid;
  atomicRelax(g_cost+g_end[tid], g_weight[tid], g_cost[g_start[tid]])
  
}
       
