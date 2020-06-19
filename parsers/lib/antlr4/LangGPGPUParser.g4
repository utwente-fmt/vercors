parser grammar LangGPGPUParser;

gpgpuLocalBarrier
    : valEmbedContract? GPGPU_BARRIER '(' GPGPU_LOCAL_BARRIER ')'
    ;

gpgpuGlobalBarrier
    : valEmbedContract? GPGPU_BARRIER '(' GPGPU_GLOBAL_BARRIER ')'
    ;

gpgpuKernelSpecifier: GPGPU_KERNEL;