package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class SYCLSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/sycl/kernels/BasicKernel.cpp"
  vercors should error withCode "multipleKernels" example "concepts/sycl/kernels/MultipleKernelsInCommandGroup.cpp"
  vercors should verify using silicon example "concepts/sycl/kernels/NDRangeKernel.cpp"
  vercors should error withCode "kernelForkPre" example "concepts/sycl/kernels/NegativeRange.cpp"
  vercors should error withCode "missingKernel" example "concepts/sycl/kernels/NoKernelInCommandGroup.cpp"
  vercors should error withCode "kernelForkPre" example "concepts/sycl/kernels/NonDivisibleNDRange.cpp"
  vercors should error withCode "incorrectParallelForLambdaArgument" example "concepts/sycl/kernels/NonMatchingRangeItem.cpp"
  vercors should error withCode "incorrectParallelForLambdaArgument" example "concepts/sycl/kernels/NonMatchingRangeItemDimensions.cpp"
  vercors should error withCode "nonSYCLCodeInCommandGroup" example "concepts/sycl/kernels/NonSYCLCodeInCommandGroup.cpp"
  vercors should error withCode "syclItemMethodPreFailed" example "concepts/sycl/kernels/TooHighKernelDimension.cpp"
  vercors should error withCode "kernelForkPre" example "concepts/sycl/kernels/ZeroNDRange.cpp"
}