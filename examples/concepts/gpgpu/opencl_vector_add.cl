#include <opencl.h>


/*@
  context get_work_dim() == 1;
  context get_local_size(0) == 32;
  context get_num_groups(0) > 0;
  context a != NULL && b != NULL && c != NULL;
  context \pointer_length(a) >= size && \pointer_length(b) >= size && \pointer_length(c) >= size;
  context (\forall* int i; 0<=i && i<4 && \gtid*4+3 < size;
    Perm({:a[\gtid*4+i]:}, 1\2) ** Perm({:1:b[\gtid*4+i]:}, 1\2) ** Perm({:2:c[\gtid*4+i]:}, 1\1));
  context (\forall* int i; 0<=i && i< size%4 && \gtid*4+3 >= size && \gtid*4 < size;
    Perm({:a[\gtid*4+i]:}, 1\2) ** Perm({:1:b[\gtid*4+i]:}, 1\2) ** Perm({:2:c[\gtid*4+i]:}, 1\1));

  ensures (\forall int i; 0<=i && i<4 && \gtid*4+3 < size;
    {:c[\gtid*4+i]:} == a[\gtid*4+i] + b[\gtid*4+i]);
  ensures (\forall int i; 0<=i && i< size%4 && \gtid*4+3 >= size && \gtid*4 < size;
    {:c[\gtid*4+i]:} == a[\gtid*4+i] + b[\gtid*4+i]);
@*/
__kernel void addArrays(__global int* a, __global int* b, __global int* c, int size) {
    int tid = get_global_id(0);
    if (4*tid+3 < size) {
        int4 a2 = vload4(tid, a);
        int4 b2 = vload4(tid, b);
        int4 c2 = a2 + b2;
        vstore4(c2, tid, c);
    } else {
      if(4*tid+2 < size) c[4*tid+2] = a[4*tid+2] + b[4*tid+2];
      if(4*tid+1 < size) c[4*tid+1] = a[4*tid+1] + b[4*tid+1]; 
      if(4*tid < size) c[4*tid] = a[4*tid] + b[4*tid];
    }
}
