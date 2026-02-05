package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class GpgpuSpec extends VercorsSpec {

  vercors should verify using silicon example "concepts/gpgpu/cuda.cu"
  vercors should verify using silicon example "concepts/gpgpu/cuda_atomic.cu"
  vercors should verify using silicon example "concepts/gpgpu/simple_vector_cuda.cu"
  vercors should verify using silicon example "concepts/gpgpu/opencl_vector_simple.cl"
  vercors should verify using silicon example "concepts/gpgpu/opencl_vector_add.cl"
  vercors should verify using silicon example "concepts/gpgpu/cuda_blur.cu"

  vercors should verify using silicon example "concepts/gpgpu/dynamic_shared_cuda.cu"
  vercors should verify using silicon example "concepts/gpgpu/dynamic_shared_opencl.cl"
  vercors should verify using silicon example "concepts/gpgpu/static_shared_cuda.cu"
  vercors should verify using silicon example "concepts/gpgpu/static_shared_opencl.cl"

  vercors should verify using silicon example "concepts/gpgpu/global_fence_opencl.cl"

  vercors should verify using silicon flags("--prover-config:smt.arith.solver=6") example "concepts/gpgpu/prefixsum-drf.pvl"
  vercors should verify using silicon flags("--prover-config:smt.arith.solver=6") example "concepts/gpgpu/summation-kernel-1.pvl"
  vercors should fail withCode "preFailed:perm" using silicon example "concepts/gpgpu/atomic_fail-1.cu"
  vercors should fail withCode "ptrPerm" using silicon example "concepts/gpgpu/atomic_fail-2.cu"

  vercors should verify using silicon example "concepts/gpgpu/xswap.cl"
  vercors should verify using silicon example "concepts/gpgpu/xtrsv.cl"
  
  vercors should error withCode "wrongGPUDimension" in "Wrong gpu dimension" c """
#include <opencl.h>


/*@
  context get_local_size(4) == 32;
@*/
__kernel void addArrays(__global int* a) {
    return;
}
    """

  vercors should fail withCode "postExtractedKernelFailed:false" using silicon in "Extract false post" c
    """
 #include <opencl.h>

/*@ extract_body
  context get_work_dim() == 1 && get_local_size(0) == 1 && get_num_groups(0) == 1;
  context a != NULL && \pointer_length(a) >= 1;
  context Perm({:a[0]:}, write);
  requires a[0] == 0;
  ensures a[0] == 1; @*/
__kernel void addArrays(__global int* a, int size) {
    int tid = get_global_id(0);
    a[tid] += 2;
}
      """


  vercors should fail withCode "callDecreasesFailed" using silicon in "Nondecreasing kernel with extracted body" c
    """
#include <opencl.h>
#include <stdlib.h>

//@ extract_body
/*@
  context get_local_size(0) == 1 && get_num_groups(0) == 1;
  context get_local_size(1) == 1 && get_num_groups(1) == 1;
  context get_local_size(2) == 1 && get_num_groups(2) == 1;
  context a != NULL && \pointer_length(a) >= 1;
  context Perm({:a[0]:}, write);
  requires a[0] == 0;
  @*/
__kernel void addArrays(__global int* a, int size) {
    int tid = get_global_id(0);
    a[tid] += 2;
}

//@ decreases;
void main() {
    int* a = (int*)malloc(sizeof(int) * 1);
    //@ assume a != NULL;
    a[0] = 0;
    // This is abviously not how you can normally call an OpenCL kernel
    // Beter host support for OpenCL would be nice
    addArrays(1, 1, 1, 1, 1, 1, a, 1);
}
"""
  // https://github.com/utwente-fmt/vercors/issues/852
  // vercors should verify using silicon example "concepts/gpgpu/GPGPU-Example-updates.cu"
  // https://github.com/utwente-fmt/vercors/issues/856
  // vercors should verify using silicon example "concepts/gpgpu/GPGPU-Example.cu"
  // vercors should verify using silicon example "concepts/gpgpu/opencl.c"
  // vercors should verify using silicon example "concepts/gpgpu/opencl_incr.c"
  // vercors should verify using silicon example "concepts/gpgpu/simple-ltid.cu"
}
