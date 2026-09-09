#include <opencl.h>


/*@
  context get_work_dim() == 1;
  context get_local_size(0) == 32;
  context get_num_groups(0) > 0;
  context a != NULL && b != NULL && c != NULL;
  context \pointer_length(a) >= size && \pointer_length(b) >= size && \pointer_length(c) >= size;
  context \gtid<size ==> Perm({:a[\gtid]:}, 1\2) ** Perm({:1:b[\gtid]:}, 1\2) ** Perm({:2:c[\gtid]:}, write);
  ensures \gtid<size ==> {:c[\gtid]:} == a[\gtid] + b[\gtid];
@*/
__kernel void addArrays(__global int* a, __global int* b, __global int* c, int size) {
    int tid = get_global_id(0);
    if (tid < size) {
      c[tid] = a[tid] + b[tid];
    }
}
