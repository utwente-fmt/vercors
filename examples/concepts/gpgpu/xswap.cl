#include "opencl.h"
// =================================================================================================
// This file adapated from the CLBlast project. Author(s):
//   Cedric Nugteren <www.cedricnugteren.nl>
//   Verification suport: Lars van den Haak
//
// This file contains kernels to perform a swap of two vectors.

/*@
  requires a > 0 && n > 0 && x >= 0 && x < n;
  ensures 0 <= x*a && x*a <= (n-1)*a;
  ensures 0 <= x*a && x*a < n*a;
  ensures x*a / a == x;
  ensures \result;
pure bool lemma1d(int x, int n, int a);

  requires a > 0 && n > 0 && x >= 0 && x < n;
  ensures lemma1d(x, n, a);
  ensures b <= \result && \result <= b + (n-1)*a
                         && \result < b + n*a;
  ensures (\result-b) / a == x;
pure int acc1d(int x, int b, int n, int a) = (x*a + b);

  // See lean: Int.tmod_eq_emod_of_nonneg and Int.add_emod_right 
  requires a >= 0;
  requires b > 0;
  ensures (a + b) % b == a % b;
  ensures \result;
  decreases;
pure bool lemma_mod(int a, int b);

 requires b != 0;
inline pure int ceil_div(int a, int b) = (a + b - 1) / b;
*/

/*@ extract_body */
/*@
  context get_local_size(0) == 32 && get_local_size(1) == 1 && get_local_size(2) == 1;
  // Making this num of groups concrete (==10) makes verification a lot faster.
  // This is better for the automated tests which needs to be faster than 300s, and otherwise fail.
  // In general this can be any get_num_groups(0)>=0, but it takes longer.
  // run with '--prover-config:smt.arith.solver=6' if using nonconcrete num of groups.
  context get_num_groups(0) == 10 && get_num_groups(1) == 1 && get_num_groups(2) == 1;
  context n >= 1;
  context x_inc >= 1 && x_offset >= 0 && xgm != NULL && \pointer_length(xgm) >= n*x_inc+x_offset;
  context y_inc >= 1 && y_offset >= 0 && ygm != NULL && \pointer_length(ygm) >= n*y_inc+y_offset;
  context (\forall* int i; 0 <= i && i < ceil_div(n, get_global_size(0))
    && \gtid + i*get_global_size(0) < n;
      Perm({:xgm[acc1d(\gtid + i*get_global_size(0), x_offset, n, x_inc)]:}, write));
  context (\forall* int i; 0 <= i && i < ceil_div(n, get_global_size(0))
    && \gtid + i*get_global_size(0) < n;
      Perm({:ygm[acc1d(\gtid + i*get_global_size(0), y_offset, n, y_inc)]:}, write));
  ensures (\forall int i; 0 <= i && i < ceil_div(n, get_global_size(0)) && \gtid + i*get_global_size(0) < n;
        {:xgm[acc1d(\gtid + i*get_global_size(0), x_offset, n, x_inc)]:} ==
        \old(ygm[acc1d(\gtid + i*get_global_size(0), y_offset, n, y_inc)]));
  ensures (\forall int i; 0 <= i && i < ceil_div(n, get_global_size(0)) && \gtid + i*get_global_size(0) < n;
        \old(xgm[acc1d(\gtid + i*get_global_size(0), x_offset, n, x_inc)]) ==
        {:ygm[acc1d(\gtid + i*get_global_size(0), y_offset, n, y_inc)]:});
@*/
// Full version of the kernel with offsets and strided accesses
__kernel void Xswap(const int n,
           /*@unique<1>*/ __global float* xgm, const int x_offset, const int x_inc,
           /*@unique<2>*/ __global float* ygm, const int y_offset, const int y_inc) {
  /*@ loop_invariant \gtid <= id && id < n + get_global_size(0);
    loop_invariant id % get_global_size(0) == \gtid;
    loop_invariant (\forall* int i; 0 <= i && i < ceil_div(n, get_global_size(0)) 
      && \gtid + i*get_global_size(0) < n;
        Perm({:xgm[acc1d(\gtid + i*get_global_size(0), x_offset, n, x_inc)]:}, write));
    loop_invariant (\forall* int i; 0 <= i && i < ceil_div(n, get_global_size(0)) 
      && \gtid + i*get_global_size(0) < n;
        Perm({:ygm[acc1d(\gtid + i*get_global_size(0), y_offset, n, y_inc)]:}, write));
    loop_invariant (\forall int i; id/get_global_size(0) <= i && i < ceil_div(n, get_global_size(0)) 
      && \gtid + i*get_global_size(0) < n;
        {:xgm[acc1d(\gtid + i*get_global_size(0), x_offset, n, x_inc)]:} ==
       \old(xgm[acc1d(\gtid + i*get_global_size(0), x_offset, n, x_inc)]));
    loop_invariant (\forall int i; id/get_global_size(0) <= i && i < ceil_div(n, get_global_size(0)) 
      && \gtid + i*get_global_size(0) < n;
        {:ygm[acc1d(\gtid + i*get_global_size(0), y_offset, n, y_inc)]:} ==
       \old(ygm[acc1d(\gtid + i*get_global_size(0), y_offset, n, y_inc)]));
    loop_invariant (\forall int i; 0 <= i && i < id/get_global_size(0)
      && \gtid + i*get_global_size(0) < n;
        {:xgm[acc1d(\gtid + i*get_global_size(0), x_offset, n, x_inc)]:} == 
        \old(ygm[acc1d(\gtid + i*get_global_size(0), y_offset, n, y_inc)]));
    loop_invariant (\forall int i; 0 <= i && i < id/get_global_size(0)
      && \gtid + i*get_global_size(0) < n;
        {:ygm[acc1d(\gtid + i*get_global_size(0), y_offset, n, y_inc)]:} == 
        \old(xgm[acc1d(\gtid + i*get_global_size(0), x_offset, n, x_inc)]));
  @*/
  // Loops over the work that needs to be done (allows for an arbitrary number of threads)
  for (int id = get_global_id(0); id<n; id += get_global_size(0)) {
    int idx = id*x_inc + x_offset;
    //@ assert acc1d(id, x_offset, n, x_inc) == idx;
    int idy = id*y_inc + y_offset;
    //@ assert acc1d(id, y_offset, n, y_inc) == idy;
    float temp = xgm[idx];
    xgm[idx] = ygm[idy];
    ygm[idy] = temp;
    // assert lemma_mod(id, get_global_size(0));
  }
}
