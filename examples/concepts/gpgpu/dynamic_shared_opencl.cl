//:: cases DynamicSharedOpenCl
//:: tool silicon
//:: verdict Pass

#include <opencl.h>

/*@
  context get_work_dim() == 1;
  context get_local_size(0) == 32;
  context get_num_groups(0) > 0;

  context in != NULL && out != NULL;
  context \pointer_length(in) == 1;
  context \pointer_length(out) == n;
  context get_local_size(0) * get_num_groups(0) >= n;
  context Perm(in[0], write \ (get_local_size(0) * get_num_groups(0)));
  context \gtid<n ==> Perm({:out[\gtid]:}, write);

  context \shared_mem_size(s) == 1;
  requires \ltid == 0 ==> Perm(s[0], write);

  ensures \gtid<n ==> {:out[\gtid]:} == \old(out[\gtid]) + in[0];
@*/
__kernel void blur_x(global int* in, global int* out, int n, local int* s) {
  int tid = get_global_id(0);
  if(get_local_id(0) == 0) {
    s[get_local_id(0)] = in[0];
  }

  /*@
    context Perm(in[0], write \ (get_local_size(0) * get_num_groups(0)));
    context \gtid<n ==> Perm({:out[\gtid]:}, write);
    context \gtid<n ==> \old(out[\gtid]) == {:out[\gtid]:};

    requires \ltid == 0 ==> Perm(s[0], write);
    requires \ltid == 0 ==> s[0] == in[0];

    ensures Perm(s[0], write \ get_local_size(0)); 

    ensures s[0] == in[0];
  @*/
  barrier(CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE);

  if(tid < n) {
    out[tid] += s[0];
  }
}
