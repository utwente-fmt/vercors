lexer grammar LangGPGPULexer;

GPGPU_BARRIER: '__vercors_barrier__';
GPGPU_LOCAL_BARRIER: '__vercors_local_barrier__';
GPGPU_GLOBAL_BARRIER: '__vercors_global_barrier__';
GPGPU_KERNEL: '__vercors_kernel__';
GPGPU_ATOMIC: '__vercors_atomic__';
GPGPU_GLOBAL_MEMORY: '__global';
GPGPU_LOCAL_MEMORY: '__local';

GPGPU_CUDA_OPEN_EXEC_CONFIG: '<<<';
GPGPU_CUDA_CLOSE_EXEC_CONFIG: '>>>';