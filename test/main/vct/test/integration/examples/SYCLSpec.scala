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
  vercors should error withCode "syclItemMethodSeqBoundExceedsLength" example "concepts/sycl/kernels/TooHighKernelDimension.cpp"
  vercors should error withCode "syclKernelRangeInvalid" example "concepts/sycl/kernels/ZeroNDRange.cpp"

  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "concepts/sycl/kernels/itemMethodsInjective/injectiveGetGlobalId.cpp"
  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "concepts/sycl/kernels/itemMethodsInjective/injectiveGetGlobalLinearId1.cpp"
  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "concepts/sycl/kernels/itemMethodsInjective/injectiveGetGlobalLinearId2.cpp"
  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "concepts/sycl/kernels/itemMethodsInjective/injectiveGetGlobalLinearId3.cpp"
  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "concepts/sycl/kernels/itemMethodsInjective/injectiveGetGroupLinearId1.cpp"
  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "concepts/sycl/kernels/itemMethodsInjective/injectiveGetGroupLinearId2.cpp"
  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "concepts/sycl/kernels/itemMethodsInjective/injectiveGetGroupLinearId3.cpp"
  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "concepts/sycl/kernels/itemMethodsInjective/injectiveGetLinearId.cpp"
  vercors should verify using silicon example "concepts/sycl/kernels/itemMethodsInjective/injectiveGetLocalLinearId1.cpp"
  vercors should verify using silicon example "concepts/sycl/kernels/itemMethodsInjective/injectiveGetLocalLinearId2.cpp"
  vercors should verify using silicon example "concepts/sycl/kernels/itemMethodsInjective/injectiveGetLocalLinearId3.cpp"

  vercors should verify using silicon example "concepts/sycl/buffers/AllBufferDimensions.cpp"
  vercors should error withCode "syclUnsupportedReassigningOfBuffer" example "concepts/sycl/buffers/NoBufferReassign.cpp"
  vercors should error withCode "noSuchName" example "concepts/sycl/buffers/UnfoldingExclusiveHostDataAccessPredicate.cpp"
  vercors should fail withCode "ptrPerm" using silicon example "concepts/sycl/buffers/ReadDataInBufferScope.cpp"
  vercors should verify using silicon example "concepts/sycl/buffers/ReleaseFromBuffer.cpp"
  vercors should error withCode "syclBufferConstructionFailed" example "concepts/sycl/buffers/TooBigBuffer.cpp"
  vercors should verify using silicon example "concepts/sycl/buffers/TooSmallBuffer.cpp"
  vercors should error withCode "syclBufferConstructionFailed" example "concepts/sycl/buffers/TwoBuffersForSameData.cpp"
  vercors should fail withCode "assignFieldFailed" using silicon example "concepts/sycl/buffers/WriteDataInBufferScope.cpp"

  vercors should error withCode "syclAccessorInsufficientReferencePermission" example "concepts/sycl/accessors/AccessorGetRangeDimensionOutOfBounds.cpp"
  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "concepts/sycl/accessors/AccessorInNDRange.cpp" // This example does verify with the flag --no-infer-heap-context-into-frame enabled
  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "concepts/sycl/accessors/AllAccessorDeclarations.cpp" // This example does verify with the flag --no-infer-heap-context-into-frame enabled
  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "concepts/sycl/accessors/DoNotWriteOnReadAccessorWithDoubleAccessors.cpp"
  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "concepts/sycl/accessors/GetKernelResult.cpp" // This example does verify with the flag --no-infer-heap-context-into-frame enabled
  vercors should error withCode "syclAccessorArraySubscriptLinearizePreconditionFailed" example "concepts/sycl/accessors/MissingRangeRequirements.cpp"
  vercors should error withCode "syclBufferOutOfScope" example  "concepts/sycl/accessors/PassBufferToMethod.cpp"
  vercors should verify using silicon example "concepts/sycl/accessors/TwoReadKernels.cpp"
  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "concepts/sycl/accessors/TwoWriteAccessorsForSameBuffer.cpp"
  vercors should verify using silicon example "concepts/sycl/accessors/TwoWriteKernels.cpp"
  vercors should verify using silicon example "concepts/sycl/accessors/TwoWriteKernelsWithWait.cpp"
  vercors should fail withCode "assignFieldFailed" using silicon example "concepts/sycl/accessors/WriteToReadAccessor.cpp"

  vercors should verify using silicon example "concepts/sycl/addressSpaces/AllLocalAccessors.cpp"
  vercors should verify using silicon example "concepts/sycl/addressSpaces/LocalAccessorUsage.cpp"
  vercors should error withCode "syclNoLocalAccessorsInBasicKernel" example "concepts/sycl/addressSpaces/LocalAccessorUsageInBasicKernel.cpp"

  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "concepts/sycl/fullExamples/MatrixTransposeWithF.cpp"
  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "concepts/sycl/fullExamples/VectorAdd.cpp"
}