//:: case GPGPUExample
/***********************************************************************************
Created by Mohsen Safari.
************************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <cuda.h>

///////// ///////////////////////////////////////////////////////////////////////
//Kernel
////////////////////////////////////////////////////////////////////////////////
/*@
context_everywhere N == blockDim.x;
context \pointer_index(g_array1, \gtid, write);
context \pointer_index(g_array2, \gtid, write);
ensures g_array1[\gtid] <= \old(g_array1[\gtid]) && g_array1[\gtid] <= g_array2[\gtid];
ensures g_array1[\gtid] == \old(g_array1[\gtid]) || g_array1[\gtid] == g_array2[\gtid];
@*/
__global__ void CUDAKernel(int* g_array1, int* g_array2, int N)
{
  int tid = blockIdx.x * N + threadIdx.x;
  //@ assert tid == \gtid;
  atomicMin(g_array1 + tid, g_array2[tid]) /*@ then { assert false && false; } */;
}

//@ ensures \pointer(\result, N, write);
int *vercorsMallocInt(int N);
void vercorsFreeInt(int *ar);
//@ ensures \pointer(\result, N, write);
int *vercorsCudaMallocInt(int N);
void vercorsCudaFreeInt(int *addr);
void vercorsCudaMemcpyInt(int *tgt, int *src, int N, int direction);

////////////////////////////////////////////////////////////////////////////////
// Main Program
////////////////////////////////////////////////////////////////////////////////
int main( int argc, char** argv) 
{

  int N = 1024; // size of the array

  // allocate host memory
  // int* host_array1 = (int*) malloc(sizeof(int)*N);
  // int* host_array2 = (int*) malloc(sizeof(int)*N);
  int* host_array1 = vercorsMallocInt(N);
  int* host_array2 = vercorsMallocInt(N);
    
  // initalize the memory
  //@ loop_invariant 0 <= i && i <= N;
  //@ loop_invariant \pointer(host_array1, N, write);
  //@ loop_invariant \pointer(host_array2, N, write);
  //@ loop_invariant (\forall int j; 0 <= j && j < i; host_array1[j] == j && host_array2[j] == N-j);
  for(unsigned int i = 0; i < N; i++) 
  {
    host_array1[i] = i;
    host_array2[i] = N-i;
  }   


  //Copy the arrays to device memory
  int* device_array1;
  device_array1 = vercorsCudaMallocInt(N);
  vercorsCudaMemcpyInt( device_array1, host_array1, N, cudaMemcpyHostToDevice) ;

  int* device_array2;
  device_array2 = vercorsCudaMallocInt(N);
  vercorsCudaMemcpyInt( device_array2, host_array2, N, cudaMemcpyHostToDevice) ;

    
  //setup execution parameters
  int num_of_blocks = 1;
  int num_of_threads_per_block = N;
  
  //dim3  grid( num_of_blocks, 1, 1); //grid has three parameters to indicate the dimensions. Here we have one dimensional grid (of blocks). It can be one, two or three dimensions.
  //dim3  threads( num_of_threads_per_block, 1, 1); //threads indicates the dimensions of one block. Here each block has one dimension (of threads). It can be one, two or three dimensions.

  //start the timer
  cudaEvent_t begin, end;
  int time;
  begin = cudaEventCreate();
  end = cudaEventCreate();
  cudaEventRecord(begin, 0);

  //Kernel launch
  CUDAKernel<<< /*grid*/num_of_blocks, /*threads*/num_of_threads_per_block/*, 0*/ >>>(device_array1, device_array2, N);

  //Stop the Timer
  cudaEventRecord(end, 0);
  cudaEventSynchronize(end);
  time = cudaEventElapsedTime(begin, end);
  cudaEventDestroy(begin);
  cudaEventDestroy(end);
  
  // copy result from device to host
  vercorsCudaMemcpyInt(host_array1, device_array1, N, cudaMemcpyDeviceToHost);

  //print kernel execution time 
  // printf( "Processing time: %d (ms)\n", time);

  // cleanup memory
  vercorsFreeInt(host_array1);
  vercorsFreeInt(host_array2);
  vercorsCudaFreeInt(device_array1);
  vercorsCudaFreeInt(device_array2);

  return 0;
       
}