//:: cases BasicOpenCL
//:: tool silicon
//:: verdict Pass

#include <opencl.h>

/*@
    context_everywhere opencl_gcount == 1;
    context \pointer_index(a, \ltid*2, write);
    context \pointer_index(a, \ltid*2+1, write);
    context \pointer_index(b, \ltid, write);
@*/
__kernel void example(int a[], int b[], int len) {
    int tid = get_local_id(0);
    a[tid*2] = tid;
    /*@
        context \pointer_index(a, \ltid*2, write);
        context \pointer_index(a, \ltid*2+1, write);
    @*/
    barrier(CLK_LOCAL_MEM_FENCE);
    a[tid*2+1] = a[tid*2] * 2;
}