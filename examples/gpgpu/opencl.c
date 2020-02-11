//:: cases BasicOpenCL
//:: verdict Pass

#include <opencl.h>

/*@
    context_everywhere opencl_gcount == 1;
    context \pointer_index(a, get_local_id(0) * 2, write);
    context \pointer_index(a, get_local_id(0) * 2 + 1, write);
@*/
__kernel void example(int a[], int len) {
    int tid = get_local_id(0);
    a[tid] = tid;
    /*@
        context \pointer_index(a, tid, write);
    @*/
    barrier(CLK_LOCAL_MEM_FENCE);
    a[tid] = a[tid] * 2;
}