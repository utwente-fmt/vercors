package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class SYCLSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/sycl/kernels/BasicKernel.cpp"
  vercors should error withCode "syclNoMultipleKernels" example "concepts/sycl/kernels/MultipleKernelsInCommandGroup.cpp"
  vercors should verify using silicon example "concepts/sycl/kernels/NDRangeKernel.cpp"
  vercors should error withCode "syclKernelRangeInvalid" example "concepts/sycl/kernels/NegativeRange.cpp"
  vercors should error withCode "syclMissingKernel" example "concepts/sycl/kernels/NoKernelInCommandGroup.cpp"
  vercors should error withCode "syclKernelRangeInvalid" example "concepts/sycl/kernels/NonDivisibleNDRange.cpp"
  vercors should error withCode "syclIncorrectParallelForLambdaArgument" example "concepts/sycl/kernels/NonMatchingRangeItem.cpp"
  vercors should error withCode "syclIncorrectParallelForLambdaArgument" example "concepts/sycl/kernels/NonMatchingRangeItemDimensions.cpp"
  vercors should error withCode "syclNoExtraCodeInCommandGroup" example "concepts/sycl/kernels/NonSYCLCodeInCommandGroup.cpp"
  vercors should fail withCode "syclItemMethodPreFailed" using silicon example "concepts/sycl/kernels/TooHighKernelDimension.cpp"
  vercors should error withCode "syclKernelRangeInvalid" example "concepts/sycl/kernels/ZeroNDRange.cpp"

  vercors should error withCode "syclAccessorInsufficientReferencePermission" example "concepts/sycl/accessors/AccessorGetRangeDimensionOutOfBounds.cpp"
  vercors should error withCode "unreachable:schematic" example "concepts/sycl/accessors/AllAccessModes.cpp" // This example does verify with the flag --no-infer-heap-context-into-frame enabled
  vercors should error withCode "unreachable:schematic" example "concepts/sycl/accessors/GetKernelResult.cpp" // This example does verify with the flag --no-infer-heap-context-into-frame enabled
  vercors should error withCode "syclAccessorArraySubscriptLinearizePreconditionFailed" example "concepts/sycl/accessors/MissingRangeRequirements.cpp"
  vercors should error withCode "syclBufferOutOfScope" example  "concepts/sycl/accessors/PassBufferToMethod.cpp"
  vercors should verify using silicon example "concepts/sycl/accessors/TwoReadKernels.cpp"
  vercors should error withCode "syclBufferLock" example "concepts/sycl/accessors/TwoWriteKernels.cpp"
  vercors should verify using silicon example "concepts/sycl/accessors/TwoWriteKernelsWithWait.cpp"
  vercors should fail withCode "assignFieldFailed" using silicon example "concepts/sycl/accessors/WriteToReadAccessor.cpp"

  vercors should verify using silicon example "concepts/sycl/buffers/AllBufferTypes.cpp"
  vercors should fail withCode "ptrPerm" using silicon example "concepts/sycl/buffers/ReadDataInBufferScope.cpp"
  vercors should verify using silicon example "concepts/sycl/buffers/ReleaseFromBuffer.cpp"
  vercors should error withCode "syclBufferConstructionFailed" example "concepts/sycl/buffers/TooBigBuffer.cpp"
  vercors should verify using silicon example "concepts/sycl/buffers/TooSmallBuffer.cpp"
  vercors should error withCode "syclBufferConstructionFailed" example "concepts/sycl/buffers/TwoBuffersForSameData.cpp"
  vercors should fail withCode "assignFieldFailed" using silicon example "concepts/sycl/buffers/WriteDataInBufferScope.cpp"
}