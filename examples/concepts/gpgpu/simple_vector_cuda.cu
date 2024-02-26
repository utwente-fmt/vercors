#include <cuda.h>
#include <vector_types.h>
#include <vector_functions.h>

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
__global__ void addArrays(int* a, int* b, int* c, int size) {
    int tid = blockIdx.x * blockDim.x + threadIdx.x;
    if (2*tid+1 < size) {
        int2 a2 = make_int2(a[2*tid], a[2*tid+1]);
        int2 b2 = make_int2(b[2*tid], b[2*tid+1]);
        int2 c2 = make_int2(a2.x+b2.x, a2.y+b2.y);
        c[2*tid] = c2.x;
        c[2*tid+1] = c2.y;
    } else if(2*tid+1 == size) {
        c[2*tid] = a[2*tid] + b[2*tid];
    }
}

