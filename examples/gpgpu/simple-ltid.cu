//:: case SimpleLtid
//:: tools silicon
//:: suite problem-fail

#include <cuda.h>

/*@
context_everywhere output != NULL;
requires opencl_gcount == 1;
requires \pointer_index(output, \ltid, 1);
@*/
__global__ void CUDA_Kernel_Blelloch(int* output)
{
}
