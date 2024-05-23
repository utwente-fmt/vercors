//:: cases DynamicSharedOpenCl
//:: tool silicon
//:: verdict Pass

#include <opencl.h>

/*@
  context get_local_size(0) == 32 && get_local_size(1) == 1 && get_local_size(2) == 1;
  context get_num_groups(0) == 4 && get_num_groups(1) == 1 && get_num_groups(2) == 1;

  context in != NULL && out != NULL;
  context \pointer_length(in) == 1;
  context \pointer_length(out) == n;
  context n > 0;
  context get_local_size(0) * get_num_groups(0) >= n;
  context Perm(&in[0], write \ (get_local_size(0) * get_num_groups(0)));
  context \gtid<n ==> Perm(&out[\gtid], write);

  context s != NULL && \pointer_length(s) == get_num_groups(0);
  requires \ltid == 0 ==> Perm(&s[get_group_id(0)], write);

  ensures \gtid<n ==> out[\gtid] == \old(out[\gtid]) + in[0];
@*/
__kernel void blur_x(global int* in, global int* out, int n, global int* s) {
  int tid = get_group_id(0) * get_local_size(0) + get_local_id(0);
  if(get_local_id(0) == 0) {
    s[get_group_id(0)] = in[0];
  }

  /*@
    context Perm(&in[0], write \ (get_local_size(0) * get_num_groups(0)));
    context get_group_id(0) * get_local_size(0) + get_local_id(0)<n ==> Perm(&out[get_group_id(0) * get_local_size(0) + get_local_id(0)], write);
    context get_group_id(0) * get_local_size(0) + get_local_id(0)<n ==> \old(out[get_group_id(0) * get_local_size(0) + get_local_id(0)]) == out[get_group_id(0) * get_local_size(0) + get_local_id(0)];

    requires get_local_id(0) == 0 ==> Perm(&s[get_group_id(0)], write);
    requires get_local_id(0) == 0 ==> s[get_group_id(0)] == in[0];

    ensures Perm(&s[get_group_id(0)], write \ get_local_size(0));

    ensures s[get_group_id(0)] == in[0];
  @*/
  barrier(CLK_GLOBAL_MEM_FENCE);

  if(tid < n) {
    out[tid] += s[get_group_id(0)];
  }
}