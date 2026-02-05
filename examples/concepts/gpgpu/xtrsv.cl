#include "opencl.h"
// =================================================================================================
// This file adapated from the CLBlast project. Author(s):
//   Cedric Nugteren <www.cedricnugteren.nl>
//   Verification suport: Lars van den Haak
//
// This file contains kernels to perform forward or backward substition, as used in the TRSV routine
//

#ifndef PRECISION
  // Data-types: half, single or double precision, complex or regular
  // Lvdh: Only verified for non-complex numbers. Otherwise annotations need to be altered.
  #define PRECISION 32
#endif
// Half-precision
#if PRECISION == 16
  typedef half real;
// Single-precision
#elif PRECISION == 32
  typedef float real;
// Double-precision 
#elif PRECISION == 64
  typedef double real;
// Complex single-precision
#elif PRECISION == 3232
  typedef float2 real;
// Complex double-precision
#elif PRECISION == 6464
  typedef double2 real;
#endif

// The block size for forward or backward substition
#define TRSV_BLOCK_SIZE 32    
//@ inline pure int TRSV_BLOCK_SIZE() = 32;
#define UNIQUE0 /*@unique<0>@*/
#define UNIQUE1 /*@unique<1>@*/
#define UNIQUE2 /*@unique<2>@*/
#define READ_ONLY const

#if PRECISION == 3232 || PRECISION == 6464
  #define COMPLEX_CONJUGATE(value) value.x = value.x; value.y = -value.y
#else
  #define COMPLEX_CONJUGATE(value) 
#endif
// The scalar multiply-subtract function
#if PRECISION == 3232 || PRECISION == 6464
  #define MultiplySubtract(c,a,b) c.x -= MulReal(a,b); c.y -= MulImag(a,b)
#else
  #define MultiplySubtract(c,a,b) c -= a * b
#endif
#if PRECISION == 3232 || PRECISION == 6464
  #define DivideFull(c,a,b) singlereal num_x = (a.x * b.x) + (a.y * b.y); singlereal num_y = (a.y * b.x) - (a.x * b.y); singlereal denom = (b.x * b.x) + (b.y * b.y); c.x = num_x / denom; c.y = num_y / denom
#else
  #define DivideFull(c,a,b) c = a / b
#endif
// Subtracts two complex variables
#if PRECISION == 3232 || PRECISION == 6464
  #define Subtract(c,a,b) c.x = a.x - b.x; c.y = a.y - b.y
#else
  #define Subtract(c,a,b) c = a - b
#endif


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

  requires n0 > 0 && x0 >= 0 && x0 < n0;
  requires n1 > 0 && x1 >= 0 && x1 < n1 &&
    a1 > 0 && a1 >= n0;
  ensures 0 <= x1*a1 + x0 && x1*a1 + x0 < a1*n1;
  ensures (x1*a1 + x0) % a1 == x0;
  ensures (x1*a1 + x0) / a1 == x1;
  ensures \result;
  decreases;
pure bool lemma2d(int x0, int x1, int n0, int n1, int a1);

  requires n0 > 0 && x0 >= 0 && x0 < n0;
  requires n1 > 0 && x1 >= 0 && x1 < n1 &&
    a1 > 0 && a1 >= n0;
  ensures lemma2d(x0, x1, n0, n1, a1);
  ensures b <= \result && \result < b + a1*n1;
  ensures (\result-b) % a1 == x0;
  ensures (\result-b) / a1 == x1;
  decreases;
pure int acc2d(int x0, int x1, int b, int n0, int n1, int a1) = (x1*a1 + x0 + b);
*/

/*@ requires 0 <= i && i<|b|;
  requires |x| == |b| && |l| == |b| && (\forall int j; 0 <= j && j < |b|; |{:l[j]:}| == |b|);
pure real xsol(seq<real> b, seq<seq<real> > l, seq<real> x, int i, bool transposed) = 
  (b[i] - x[i] - xsolh(b, l, x, i, i, transposed))/l[i][i];

  requires 0 <= i && i<|b|;
  requires 0 <= j && j<=i;
  requires |x| == |b| && |l| == |b| && (\forall int j; 0 <= j && j <|b|; |{:l[j]:}| == |b|);
pure real xsolh(seq<real> b, seq<seq<real> > l, seq<real> x, int i, int j, bool transposed) = 
  (j==0) ? 0 :
  (\let real _l = transposed ? l[j-1][i] : l[i][j-1];
  _l * xsol(b, l, x, j-1, transposed) + xsolh(b, l, x, i, j-1, transposed)
  );@*/

/*@ extract_body */
/*@
  given seq<real> _x;
  given seq<real> _b;
  given seq<seq<real > > _A;

  context get_num_groups(0) == 1 && get_num_groups(1) == 1 && get_num_groups(2) == 1;
  context get_local_size(0) == TRSV_BLOCK_SIZE() && get_local_size(1) == 1 && get_local_size(2) == 1;
  context n >= 1 && n <= TRSV_BLOCK_SIZE();
  context x_inc >= 1 && x_offset >= 0;
  context b_inc >= 1 && b_offset >= 0;
  context x != NULL && \pointer_length(x) >= n*x_inc+x_offset;
  context b != NULL && \pointer_length(b) >= n*b_inc+b_offset;
  context A != NULL && a_offset >= 0;
  context a_ld >= n && \pointer_length(A) >= a_ld * n + a_offset;
  
  context \ltid < n ==> Perm({:x[acc1d(\ltid, x_offset, n, x_inc)]:}, write);*/
  /*@context |_x| == n;
  context \ltid < n ==> \old(x[acc1d(\ltid, x_offset, n, x_inc)]) == {:_x[\ltid]:};
  context |_b| == n;
  context \ltid < n ==> (b[acc1d(\ltid, b_offset, n, b_inc)]) == {:_b[\ltid]:};
  context |_A| == n && (\forall int i; 0 <= i && i<n; |{:_A[i]:}| == n);
  context (\forall int i, int j; 0 <= i && i<n && 0 <= j && j<n;
    A[acc2d(j, i, a_offset, a_ld, n, a_ld)] == {:_A[j][i]:});
  context is_unit_diagonal != 0 ==> (\forall int i; 0 <= i && i<n; {:_A[i][i]:} == 1);

  context (\forall* int i; 0 <= i && i<TRSV_BLOCK_SIZE(); Perm({:alm[i][\ltid]:}, write));
  context Perm({:xlm[\ltid]:}, write);
  ensures \ltid < n ==> {:x[acc1d(\ltid, x_offset, n, x_inc)]:} == xsol(_b, _A, _x, \ltid, is_transposed!=0); */
__kernel void trsv_forward(int n,
                  READ_ONLY __global real *A, const int a_offset, int a_ld,
                  READ_ONLY __global real *b, const int b_offset, int b_inc,
                  UNIQUE0 __global real *x, const int x_offset, int x_inc,          
                  const int is_transposed, const int is_unit_diagonal, const int do_conjugate) {
  UNIQUE1 __local real alm[TRSV_BLOCK_SIZE][TRSV_BLOCK_SIZE];
  UNIQUE2 __local real xlm[TRSV_BLOCK_SIZE];
  const int tid = get_local_id(0);
  
  // Pre-loads the data into local memory
  if (tid < n) {
    int bid = tid*b_inc + b_offset;
    int xid = tid*x_inc + x_offset;
    //@ assert bid == acc1d(tid, b_offset, n, b_inc);
    //@ assert xid == acc1d(tid, x_offset, n, x_inc);
    Subtract(xlm[tid], b[bid], x[xid]);
    if (is_transposed == 0) {
      /*@ loop_invariant 0 <= i && i <= n;
        loop_invariant (\forall* int j; 0 <= j && j<n; Perm({:alm[j][\ltid]:}, write));
        loop_invariant (\forall int i, int j; 0 <= i && i<n && 0 <= j && j<n;
          \old(A[acc2d(j, i, a_offset, a_ld, n, a_ld)]) == {:_A[j][i]:});
        loop_invariant (\forall* int j; 0 <= j && j<i; {:alm[j][\ltid]:} == _A[j][\ltid]);
      */
      for (int i = 0; i < n; ++i) {
        int id = i + tid*a_ld + a_offset;
        //@ assert id == acc2d(i, tid, a_offset, a_ld, n, a_ld);
        alm[i][tid] = A[id];
      }
    }
    else {
      /*@ loop_invariant 0 <= i && i <= n;
        loop_invariant (\forall* int j; 0 <= j && j<n; Perm({:alm[j][\ltid]:}, write));
        loop_invariant (\forall int i, int j; 0 <= i && i<n && 0 <= j && j<n;
          \old(A[acc2d(j, i, a_offset, a_ld, n, a_ld)]) == {:_A[j][i]:});
        loop_invariant (\forall* int j; 0 <= j && j<i; {:alm[j][\ltid]:} == _A[\ltid][j]);*/
      for (int i = 0; i < n; ++i) {
        int id = tid + i*a_ld + a_offset;
        //@ assert id == acc2d(tid, i, a_offset, a_ld, n, a_ld);
        alm[i][tid] = A[id];
      }
    }
    if (do_conjugate != 0) {
      /*@ loop_invariant 0 <= i && i <= n;
        loop_invariant (\forall* int j; 0 <= j && j<n; Perm({:alm[j][\ltid]:}, write));
        loop_invariant (\forall* int j; i <= j && j<n; {:alm[j][\ltid]:} == (is_transposed!=0 ?_A[\ltid][j] : _A[j][\ltid]));
        // Change for complex numbers
        loop_invariant (\forall* int j; 0 <= j && j<i; {:alm[j][\ltid]:} == (is_transposed!=0 ?_A[\ltid][j] : _A[j][\ltid]));*/
      for (int i = 0; i < n; ++i) {
        COMPLEX_CONJUGATE(alm[i][tid]);
      }
    }
  }
  
  /*@ requires (\forall* int j; \ltid<n && 0 <= j && j<n; Perm({:alm[j][\ltid]:}, 1\2));
  requires (\forall int j; \ltid<n && 0 <= j && j<n; {:alm[j][\ltid]:} == (is_transposed!=0 ?_A[\ltid][j] : _A[j][\ltid]));
  ensures (\forall* int i, int j; \ltid==0 && 0 <= i && i<n && 0 <= j && j<n; Perm({:alm[j][i]:}, 1\2));
  ensures (\forall int i, int j; \ltid==0 && 0 <= i && i<n && 0 <= j && j<n; {:alm[j][i]:}== (is_transposed!=0 ?_A[i][j] : _A[j][i]));

  requires \ltid<n ==> Perm({:xlm[\ltid]:}, write);
  requires \ltid<n ==> {:xlm[\ltid]:} == _b[\ltid] - _x[\ltid];
  ensures (\forall* int j; \ltid==0 && 0 <= j && j<n;  Perm({:xlm[j]:}, write));
  ensures (\forall int j; \ltid==0 && 0 <= j && j<n; {:xlm[j]:} == _b[j] - _x[j]);*/
  barrier(CLK_LOCAL_MEM_FENCE);

  // Computes the result (single-threaded for now)
  if (tid == 0) {
    //@ extract
    /*@ loop_invariant 0 <= i && i <= n;
      loop_invariant n <= TRSV_BLOCK_SIZE();
      loop_invariant |_b| == n && |_x| == n && |_A| == n && (\forall int i; 0 <= i && i<n; |{:_A[i]:}| == n);
      loop_invariant (\forall* int j; 0 <= j && j<n; Perm({:xlm[j]:}, write));
      loop_invariant (\forall* int j, int k; 0 <= j && j<n && 0 <= k && k<n; Perm({:alm[j][k]:}, 1\2));
      loop_invariant (\forall int i, int j; 0 <= i && i<n && 0 <= j && j<n; 
        {:alm[j][i]:}== (is_transposed!=0 ?_A[i][j] : _A[j][i]));
      loop_invariant is_unit_diagonal != 0 ==> (\forall int i; 0 <= i && i<n; {:_A[i][i]:} == 1);
      loop_invariant (\forall int j; i <= j && j<n; {:xlm[j]:} == _b[j] - _x[j]);
      loop_invariant (\forall int j; 0 <= j && j<i && j<n; {:xlm[j]:} == xsol(_b, _A, _x, j, is_transposed!=0));*/
    for (int i = 0; i < n; ++i) {
      /*@ loop_invariant 0 <= j && j <= i && 0 <= i && i < n;
        loop_invariant Perm(xlm[i], write);
        loop_invariant (\forall* int k; 0 <= k && k<i; Perm({:xlm[k]:}, 1\2));
        loop_invariant (\forall int k; 0 <= k && k<i && k<n; {:xlm[k]:} == xsol(_b, _A, _x, k, is_transposed!=0));
        loop_invariant (\forall* int j, int k; 0 <= j && j<n && 0 <= k && k<n; Perm({:alm[j][k]:}, 1\2));
        loop_invariant (\forall int i, int j; 0 <= i && i<n && 0 <= j && j<n; 
          {:alm[j][i]:}== (is_transposed!=0 ?_A[i][j] : _A[j][i]));
        loop_invariant {:xlm[i]:} == _b[i] - _x[i] - xsolh(_b, _A, _x, i, j, is_transposed!=0);*/
      for (int j = 0; j < i; ++j) {
        MultiplySubtract(xlm[i], alm[i][j], xlm[j]);
      }
      if (is_unit_diagonal == 0) { 
        DivideFull(xlm[i], xlm[i], alm[i][i]); 
      }
    }
  }

  /*@ requires (\forall* int i, int j; \ltid==0 && 0 <= i && i<n && 0 <= j && j<n; Perm({:alm[i][j]:}, 1\2));
    ensures (\forall* int i; \ltid<n && 0 <= i && i<n; Perm({:alm[i][\ltid]:}, 1\2));
    requires (\forall* int j; \ltid==0 && 0 <= j && j<n;  Perm({:xlm[j]:}, write));
    requires (\forall int j; \ltid==0 && 0 <= j && j<n; {:xlm[j]:} == xsol(_b, _A, _x, j, is_transposed!=0));
    ensures \ltid<n ==> Perm({:xlm[\ltid]:}, write);
    ensures \ltid<n ==> {:xlm[\ltid]:} == xsol(_b, _A, _x, \ltid, is_transposed!=0);*/
  barrier(CLK_LOCAL_MEM_FENCE);

  // Stores the results
  if (tid < n) {
    int xid = tid*x_inc + x_offset;
    //@ assert xid == acc1d(tid, x_offset, n, x_inc);
    x[xid] = xlm[tid];
  }
}
