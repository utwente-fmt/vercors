parser grammar LangGPGPUParser;

gpgpuLocalBarrier
    : valEmbedContract? GPGPU_BARRIER '(' GPGPU_LOCAL_BARRIER ')'
    ;

gpgpuGlobalBarrier
    : valEmbedContract? GPGPU_BARRIER '(' GPGPU_GLOBAL_BARRIER ')'
    ;

gpgpuCudaKernelInvocation
    : clangIdentifier GPGPU_CUDA_OPEN_EXEC_CONFIG expression ',' expression GPGPU_CUDA_CLOSE_EXEC_CONFIG '(' argumentExpressionList ')'
    ;

gpgpuKernelSpecifier: GPGPU_KERNEL;