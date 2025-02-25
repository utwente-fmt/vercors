#include <opencl.h>


/*@
  context get_local_size(0) > 0 && get_local_size(1) == 1 && get_local_size(2) == 1;
  context get_num_groups(0) > 0 && get_num_groups(1) == 1 && get_num_groups(2) == 1;
  context a != NULL && b != NULL && c != NULL;
  context \pointer_length(a) >= size && \pointer_length(b) >= size && \pointer_length(c) >= size;
  context \gtid*2 < size ==> (Perm(&a[\gtid*2], 1\2) ** Perm(&b[\gtid*2], 1\2) ** Perm(&c[\gtid*2], 1\1));
  context \gtid*2+1 < size ==> (Perm(&a[\gtid*2+1], 1\2) ** Perm(&b[\gtid*2+1], 1\2) ** Perm(&c[\gtid*2+1], 1\1));
  ensures \gtid*2 < size ==> c[\gtid*2] == a[\gtid*2] + b[\gtid*2];
  ensures \gtid*2+1 < size ==> c[\gtid*2+1] == a[\gtid*2+1] + b[\gtid*2+1];
@*/
__kernel void addArrays(__global int* a, __global int* b, __global int* c, int size) {
    int tid = get_global_id(0);
    if (2*tid+1 < size) {
        // int2 a2 = (int2)(a[2*tid], a[2*tid+1]);
        int2 a2 = vload2(tid, a) /*@ given {r=1\2} @*/;
        // int2 b2 = (int2)(b[2*tid], b[2*tid+1]);
        int2 b2 = vload2(tid, b) /*@ given {r=1\2} @*/;
        int2 c2 = a2 + b2;
        vstore2(c2, tid, c);
    } else if(2*tid+1 == size) {
        c[2*tid] = a[2*tid] + b[2*tid];
    }
}