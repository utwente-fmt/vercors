#include <opencl.h>

/*@ pure @*/ bool check2(int2 v) {
  return v.s0 != 0 && v.s1 != 0;
}

/*@ pure @*/ bool check4(int4 v) {
  return v.s0 != 0 && v.s1 != 0 && v.s2 != 0 && v.s3 != 0;
}

/*@ pure @*/ bool check8(int8 v) {
  return v.s0 != 0 && v.s1 != 0 && v.s2 != 0 && v.s3 != 0
    && v.s4 != 0 && v.s5 != 0 && v.s6 != 0 && v.s7 != 0;
}

/*@ pure @*/ bool check16(int16 v) {
  return v.s0 != 0 && v.s1 != 0 && v.s2 != 0 && v.s3 != 0
    && v.s4 != 0 && v.s5 != 0 && v.s6 != 0 && v.s7 != 0
    && v.s8 != 0 && v.s9 != 0 && v.sA != 0 && v.sB != 0
    && v.sC != 0 && v.sD != 0 && v.sE != 0 && v.sF != 0;
}

/*@
  context get_local_size(0) > 0 && get_local_size(1) == 1 && get_local_size(2) == 1;
  context get_num_groups(0) > 0 && get_num_groups(1) == 1 && get_num_groups(2) == 1;
  context a != NULL && \pointer_length(a) >= 1;
  context (\gtid < 1 ==> Perm(&a[\gtid], write));
@*/
__kernel void test_vectors(__global bool* a) {
  int2 v_iA = (int2)(7, -3);
  int4 v_iB = (int4)(v_iA, v_iA);
  bool res01 = check4(v_iB.xyzw == (int4)(7, -3, 7, -3));
  bool res02 = check4(v_iB.s0123 == (int4)(7, -3, 7, -3));
  bool res03 = check4(v_iB.s0123 == (int4)(7, -3, 7, -3));
  int16 v16 = (int16)(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
  int16 v16_2 = (int16)(v_iB, v_iB, 9, 9, v_iA, 9, 9, v_iA);
  bool res04 = check16(v16.s0123456789ABCDEF == (int16)(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15));
  bool res05 = check16(v16.s0123456789abcdef == (int16)(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15));
  bool res06 = check8(v16.odd == (int8)(1, 3, 5, 7, 9, 11, 13, 15));
  bool res07 = check8(v16.even == (int8)(0, 2, 4, 6, 8, 10, 12, 14));
  bool res08 = check8(v16.lo == (int8)(0, 1, 2, 3, 4, 5, 6, 7));
  bool res09 = check8(v16.hi == (int8)(8, 9, 10, 11, 12, 13, 14, 15));
  v_iA.xy = (int2)(1, 2);
  v_iA.yx = v_iA.xy;
  bool res10 = check2(v_iA == (int2)(2,1));
  v_iB.lo = v_iB.lo + (int2)(1, 2);
  bool res11 = check4(v_iB == (int4)(8, -1, 7, -3));
  v_iB.hi = v_iB.hi - (int2)(3, 4);
  bool res12 = check4(v_iB == (int4)(8, -1, 4, -7));
  v_iB.odd = v_iB.odd * (int2)(-1, -1);
  bool res13 = check4(v_iB == (int4)(8, 1, 4, 7));
  v_iB.even = v_iB.even / (int2)(-3, -2);
  bool res14 = check4(v_iB == (int4)(-2, 1, -2, 7));
  v_iB.s12 = v_iB.s21 % (int2)(-3, -3);
  bool res15 = check4(v_iB == (int4)(-2, -2, 1, 7));
  // bool res16 = check4((float4)(0.0f, 0.0f, 0.0f, 0.0f) == (float4)(0.0f, 0.0f, 0.0f, 1.0f));
  bool total = res01 && res02 && res03 && res04 && res05 && res06 && res07 
    && res08 && res09 && res10 && res11 && res12 && res13 && res14 && res15;
  //@ assert total;
  if(get_global_id(0) < 1){
    a[get_global_id(0)] = total  ;
  }
}