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
  given int contrib;
  kernel_invariant Perm(g_sum, 1) ** (\forall* int i; i >= 0 && i < N; Perm(contrib[i], 1\2)) ** g_sum == (\sum int i; 0 <= i && i < N; contrib[i]);
  requires Perm(g_array[\gtid], 1);
  requires (\forall* int i; i >= 0 && i < N; Perm(contrib[i], 1\2));
  requires contrib[\gtid] == 0;
  ensures contrib[\gtid] == g_array[\gtid];
  ensures (\forall* int i; i >= 0 && i < N; Perm(contrib[i], 1\2));
  ensures Perm(g_array[\gtid], 1);
@*/
__global__ void CUDAKernel(int* g_array, int* g_sum, int N)
{
  int tid = blockIdx.x * N + threadIdx.x;
  // each thread atomicaly adds its own value into g_sum
  atomicAdd(&g_sum, g_array[tid]) /*@ then {contrib[\gtid] = g_array[\gtid];} @*/;
}

////////////////////////////////////////////////////////////////////////////////
// Main Program
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char** argv)
{

  int N = 1024; // size of the array

  // allocate host memory
  int* host_array = (int*) malloc(sizeof(int)*N);
  int host_sum = 0;
  // initalize the memory
  for(unsigned int i = 0; i < N; i++)
  {
    host_array[i] = i;
  }


  //Copy the array to device memory
  int* device_array;
  cudaMalloc( (void**) &device_array, sizeof(int)*N) ;
  cudaMemcpy( device_array, host_array, sizeof(int)*N, cudaMemcpyHostToDevice) ;

  //Copy the int variable to device memory
  int* device_sum;
  cudaMalloc((void**) &device_sum, sizeof(int));
  cudaMemcpy( device_sum, host_sum, sizeof(int), cudaMemcpyHostToDevice);


  //setup execution parameters
  int num_of_blocks = 1;
  int num_of_threads_per_block = N;

  //start the timer
  cudaEvent_t begin, end;
  float time;
  cudaEventCreate(&begin);
  cudaEventCreate(&end);
  cudaEventRecord(begin, 0);

  /*@ ghost int contrib[N] = 0; @*/

  //Kernel launch
  CUDAKernel<<< num_of_blocks, num_of_threads_per_block >>>(device_array, device_sum, N) /*@ with { contrib = contrib; } @*/;

  //Stop the Timer
  cudaEventRecord(end, 0);
  cudaEventSynchronize(end);
  cudaEventElapsedTime(&time, begin, end);
  cudaEventDestroy(begin);
  cudaEventDestroy(end);

  // copy result from device to host
  cudaMemcpy(host_sum, device_sum, sizeof(int), cudaMemcpyDeviceToHost);

  //print kernel execution time
  printf( "Processing time: %f (ms)\n", time);

  //print the result
  printf( "Sum of the array values: %d \n", host_sum);

  // cleanup memory
  free(host_array);
  cudaFree(device_array);

  return 0;

}
