lexer grammar LangGPGPULexer;

GPGPU_BARRIER: '__vercors_barrier__';
GPGPU_LOCAL_MEMORY_FENCE: '__vercors_local_mem_fence__';
GPGPU_GLOBAL_MEMORY_FENCE: '__vercors_global_mem_fence__';
GPGPU_KERNEL: '__vercors_kernel__';
GPGPU_ATOMIC: '__vercors_atomic__';
GPGPU_GLOBAL_MEMORY: '__vercors_global_memory__';
GPGPU_LOCAL_MEMORY: '__vercors_local_memory__';

GPGPU_CUDA_OPEN_EXEC_CONFIG: '<<<';
GPGPU_CUDA_CLOSE_EXEC_CONFIG: '>>>';