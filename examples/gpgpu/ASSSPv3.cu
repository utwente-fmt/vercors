/***********************************************************************************
Created by Mohsen Safari.
************************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <cuda.h>

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

  kernel_invariant \pointer(g_start, A, 1\A);
  kernel_invariant \pointer(g_end, A, 1\A);
  kernel_invariant \pointer(g_weight, A, 1\A);
  kernel_invariant \pointer(g_cost, A, 1);

  requires \pointer_index(g_start, \gtid, 1\A);
  requires \pointer_index(g_end, \gtid, 1\A);
  requires \pointer_index(g_weight, \gtid, 1\A);
  requires \pointer_index(g_cost, \gtid, write);
@*/
__global__ void CUDAKernel(int* g_start, int* g_end, int* g_weight, int* g_cost, int V, int A, int counter, int source)
{
  int tid = blockIdx.x * V + threadIdx.x;
  //@ assert tid == \gtid;
  //@ assert 0 <= tid && tid < A;
  atomicRelax(g_cost+g_end[tid], g_weight[tid], g_cost[g_start[tid]]);
}
