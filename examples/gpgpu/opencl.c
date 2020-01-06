//:: cases BasicOpenCL
//:: verdict Pass

#include <opencl.h>

/*@
    context_everywhere opencl_gcount == 1;
    context \pointer_index(a, get_local_id(0), write);
@*/
__kernel void example(int a[], int len) {
    int tid = get_local_id(0);
    a[tid] = tid;
    /*@
        context \pointer_index(a, get_local_id(0), write);
    @*/
    barrier(CLK_LOCAL_MEM_FENCE);
    a[tid] = a[tid] * 2;
}