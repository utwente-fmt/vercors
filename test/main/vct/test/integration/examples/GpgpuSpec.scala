package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class GpgpuSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/gpgpu/cuda.cu"
  vercors should verify using silicon example "concepts/gpgpu/static_shared_cuda.cu"
  vercors should verify using silicon example "concepts/gpgpu/static_shared_opencl.cl"
  // https://github.com/utwente-fmt/vercors/issues/852
  // vercors should verify using silicon example "concepts/gpgpu/GPGPU-Example-updates.cu"
  // https://github.com/utwente-fmt/vercors/issues/856
  // vercors should verify using silicon example "concepts/gpgpu/GPGPU-Example.cu"
  // vercors should verify using silicon example "concepts/gpgpu/opencl.c"
  // vercors should verify using silicon example "concepts/gpgpu/opencl_incr.c"
  // vercors should verify using silicon example "concepts/gpgpu/simple-ltid.cu"
}
