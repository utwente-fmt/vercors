parser grammar LangGPGPUParser;

gpgpuBarrier
    : valEmbedContract? GPGPU_BARRIER '(' gpgpuMemFenceList ')'
    ;

gpgpuCudaKernelInvocation
    : clangIdentifier GPGPU_CUDA_OPEN_EXEC_CONFIG expression ',' expression GPGPU_CUDA_CLOSE_EXEC_CONFIG '(' argumentExpressionList ')' valEmbedGiven? valEmbedYields?
    ;

gpgpuAtomicBlock
    : valEmbedWith? GPGPU_ATOMIC compoundStatement valEmbedThen?
    ;

gpgpuMemFenceList
    :   gpgpuMemFence
    |   gpgpuMemFenceList '|' gpgpuMemFence
    ;

gpgpuMemFence
    :   GPGPU_LOCAL_MEMORY_FENCE
    |   GPGPU_GLOBAL_MEMORY_FENCE
    |   Constant
    ;

gpgpuKernelSpecifier
    : CUDA_KERNEL
    | OPENCL_KERNEL
    ;

gpgpuLocalMemory: GPGPU_LOCAL_MEMORY;

gpgpuGlobalMemory: GPGPU_GLOBAL_MEMORY;