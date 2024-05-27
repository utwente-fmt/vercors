#ifndef OPENCL_H
#define OPENCL_H

#define __kernel __opencl_kernel__

#define CLK_GLOBAL_MEM_FENCE __vercors_global_mem_fence__
#define CLK_LOCAL_MEM_FENCE __vercors_local_mem_fence__

#define barrier(locality) __vercors_barrier__(locality)

#define __global __vercors_global_memory__
#define global __vercors_global_memory__
#define __local __vercors_local_memory__
#define local __vercors_local_memory__

#define bool _Bool

extern int /*@ pure @*/ get_work_dim(); // Number of dimensions in use

extern int /*@ pure @*/ get_global_size(int dimindx); // Number of global work-items

extern int /*@ pure @*/ get_global_id (int dimindx); // Global work-item ID value

extern int /*@ pure @*/ get_local_size (int dimindx); //

extern int /*@ pure @*/ get_enqueued_local_size (int dimindx); // Number of local work-items

extern int /*@ pure @*/ get_local_id (int dimindx); // Local work-item ID

extern int /*@ pure @*/ get_num_groups (int dimindx); // Number of work-groups

extern int /*@ pure @*/ get_group_id (int dimindx); // Work-group ID

extern int /*@ pure @*/ get_global_offset (int dimindx); // Global offset

extern int /*@ pure @*/ get_global_linear_id (); // Work-items, 1-dimensional global ID

extern int /*@ pure @*/ get_local_linear_id (); // Work-items, 1-dimensional local ID

extern int /*@ pure @*/ get_sub_group_size (); // Number of work-items in the subgroup

extern int /*@ pure @*/ get_max_sub_group_size (); // Maximum size of a subgroup

extern int /*@ pure @*/ get_num_sub_groups (); // Number of subgroups

extern int /*@ pure @*/ get_enqueued_num_sub_groups (); //

extern int /*@ pure @*/ get_sub_group_id (); // Sub-group ID

extern int /*@ pure @*/ get_sub_group_local_id (); // Unique work-item ID

/* For vector types:
  See https://registry.khronos.org/OpenCL/specs/3.0-unified/html/OpenCL_C.html#built-in-vector-data-types

Internally we use the custom `__opencl_vector_type__` type to define them.
*/

typedef __opencl_vector_type__(int, 2) int2;
typedef __opencl_vector_type__(int, 3) int3;
typedef __opencl_vector_type__(int, 4) int4;
typedef __opencl_vector_type__(int, 8) int8;
typedef __opencl_vector_type__(int, 16) int16;

typedef __opencl_vector_type__(unsigned int, 2) uint2;
typedef __opencl_vector_type__(unsigned int, 3) uint3;
typedef __opencl_vector_type__(unsigned int, 4) uint4;
typedef __opencl_vector_type__(unsigned int, 8) uint8;
typedef __opencl_vector_type__(unsigned int, 16) uint16;

typedef __opencl_vector_type__(long, 2) long2;
typedef __opencl_vector_type__(long, 3) long3;
typedef __opencl_vector_type__(long, 4) long4;
typedef __opencl_vector_type__(long, 8) long8;
typedef __opencl_vector_type__(long, 16) long16;

typedef __opencl_vector_type__(unsigned long, 2) ulong2;
typedef __opencl_vector_type__(unsigned long, 3) ulong3;
typedef __opencl_vector_type__(unsigned long, 4) ulong4;
typedef __opencl_vector_type__(unsigned long, 8) ulong8;
typedef __opencl_vector_type__(unsigned long, 16) ulong16;

typedef __opencl_vector_type__(float, 2) float2;
typedef __opencl_vector_type__(float, 3) float3;
typedef __opencl_vector_type__(float, 4) float4;
typedef __opencl_vector_type__(float, 8) float8;
typedef __opencl_vector_type__(float, 16) float16;

typedef __opencl_vector_type__(double, 2) double2;
typedef __opencl_vector_type__(double, 3) double3;
typedef __opencl_vector_type__(double, 4) double4;
typedef __opencl_vector_type__(double, 8) double8;
typedef __opencl_vector_type__(double, 16) double16;


// For the definition of load and stores see:
// https://registry.khronos.org/OpenCL/specs/3.0-unified/html/OpenCL_C.html#vector-data-load-and-store-functions

/*@
  given rational r;
  context p != NULL;
  context r > 0\1 && r < write;
  context offset >= 0;
  context \pointer_length(p) >= offset*2+2;
  context Perm(&p[offset*2], r) ** Perm(&p[offset*2+1], r);
  ensures \result.s0 == p[offset*2] && \result.s1 == p[offset*2+1];
@*/
int2 vload2(int offset, int* p);

/*@
  given rational r;
  context p != NULL;
  context r > 0\1 && r < write;
  context offset >= 0;
  context \pointer_length(p) >= offset*2+3;
  context Perm(&p[offset*2], r) ** Perm(&p[offset*2+1], r) ** Perm(&p[offset*2+2], r);
  ensures \result.s0 == p[offset*2] && \result.s1 == p[offset*2+1] && \result.s2 == p[offset*2+2];
@*/
int3 vload3(int offset, int* p);

/*@
  given rational r;
  context p != NULL;
  context r > 0\1 && r < write;
  context offset >= 0;
  context \pointer_length(p) >= offset*2+4;
  context Perm(&p[offset*2], r) ** Perm(&p[offset*2+1], r) ** Perm(&p[offset*2+2], r) ** Perm(&p[offset*2+3], r);
  ensures \result.s0 == p[offset*2] && \result.s1 == p[offset*2+1] && \result.s2 == p[offset*2+2]
      && \result.s3 == p[offset*2+3];
@*/
int4 vload4(int offset, int* p);

/*@
  given rational r;
  context p != NULL;
  context r > 0\1 && r < write;
  context offset >= 0;
  context \pointer_length(p) >= offset*2+8;
  context Perm(&p[offset*2], r) ** Perm(&p[offset*2+1], r) ** Perm(&p[offset*2+2], r) ** Perm(&p[offset*2+3], r)
     ** Perm(&p[offset*2+4], r) ** Perm(&p[offset*2+5], r) ** Perm(&p[offset*2+6], r) ** Perm(&p[offset*2+7], r);
  ensures \result.s0 == p[offset*2] && \result.s1 == p[offset*2+1] && \result.s2 == p[offset*2+2]
      && \result.s3 == p[offset*2+3] && \result.s4 == p[offset*2+4] && \result.s5 == p[offset*2+5]
      && \result.s6 == p[offset*2+6] && \result.s7 == p[offset*2+7];
@*/
int8 vload8(int offset, int* p);

/*@
  given rational r;
  context p != NULL;
  context r > 0\1 && r < write;
  context offset >= 0;
  context \pointer_length(p) >= offset*2+16;
  context Perm(&p[offset*2], r) ** Perm(&p[offset*2+1], r) ** Perm(&p[offset*2+2], r) ** Perm(&p[offset*2+3], r)
     ** Perm(&p[offset*2+4], r) ** Perm(&p[offset*2+5], r) ** Perm(&p[offset*2+6], r) ** Perm(&p[offset*2+7], r)
     ** Perm(&p[offset*2+8], r) ** Perm(&p[offset*2+9], r) ** Perm(&p[offset*2+10], r) ** Perm(&p[offset*2+11], r)
     ** Perm(&p[offset*2+12], r) ** Perm(&p[offset*2+13], r) ** Perm(&p[offset*2+14], r) ** Perm(&p[offset*2+15], r);
  ensures \result.s0 == p[offset*2] && \result.s1 == p[offset*2+1] && \result.s2 == p[offset*2+2]
      && \result.s3 == p[offset*2+3] && \result.s4 == p[offset*2+4] && \result.s5 == p[offset*2+5]
      && \result.s6 == p[offset*2+6] && \result.s7 == p[offset*2+7] && \result.s8 == p[offset*2+8]
      && \result.s9 == p[offset*2+9] && \result.sA == p[offset*2+10] && \result.sB == p[offset*2+11]
      && \result.sC == p[offset*2+12] && \result.sD == p[offset*2+13] && \result.sE == p[offset*2+14]
      && \result.sF == p[offset*2+15];
@*/
int16 vload16(int offset, int* p);

/*@
  context p != NULL;
  context offset >= 0;
  context \pointer_length(p) >= offset*2+2;
  context Perm(&p[offset*2], write) ** Perm(&p[offset*2+1], write);
  ensures data.s0 == p[offset*2] && data.s1 == p[offset*2+1];
@*/
void vstore2(int2 data, int offset, int* p);

/*@
  context p != NULL;
  context offset >= 0;
  context \pointer_length(p) >= offset*2+3;
  context Perm(&p[offset*2], write) ** Perm(&p[offset*2+1], write) ** Perm(&p[offset*2+2], write);
  ensures data.s0 == p[offset*2] && data.s1 == p[offset*2+1] && data.s2 == p[offset*2+2];
@*/
void vstore3(int3 data, int offset, int* p);

/*@
  context p != NULL;
  context offset >= 0;
  context \pointer_length(p) >= offset*2+4;
  context Perm(&p[offset*2], write) ** Perm(&p[offset*2+1], write) ** Perm(&p[offset*2+2], write)
     ** Perm(&p[offset*2+3], write);
  ensures data.s0 == p[offset*2] && data.s1 == p[offset*2+1] && data.s2 == p[offset*2+2]
          && data.s3 == p[offset*2+3];
@*/
void vstore4(int4 data, int offset, int* p);

/*@
  context p != NULL;
  context offset >= 0;
  context \pointer_length(p) >= offset*2+8;
  context Perm(&p[offset*2], write) ** Perm(&p[offset*2+1], write) ** Perm(&p[offset*2+2], write)
     ** Perm(&p[offset*2+3], write) ** Perm(&p[offset*2+4], write) ** Perm(&p[offset*2+5], write)
     ** Perm(&p[offset*2+6], write) ** Perm(&p[offset*2+7], write);
  ensures data.s0 == p[offset*2] && data.s1 == p[offset*2+1] && data.s2 == p[offset*2+2]
          && data.s3 == p[offset*2+3] && data.s4 == p[offset*2+4] && data.s5 == p[offset*2+5]
          && data.s6 == p[offset*2+6] && data.s7 == p[offset*2+7];
@*/
void vstore8(int8 data, int offset, int* p);

/*@
  context p != NULL;
  context offset >= 0;
  context \pointer_length(p) >= offset*2+16;
  context Perm(&p[offset*2], write) ** Perm(&p[offset*2+1], write) ** Perm(&p[offset*2+2], write)
     ** Perm(&p[offset*2+3], write) ** Perm(&p[offset*2+4], write) ** Perm(&p[offset*2+5], write)
     ** Perm(&p[offset*2+6], write) ** Perm(&p[offset*2+7], write) ** Perm(&p[offset*2+8], write)
     ** Perm(&p[offset*2+9], write) ** Perm(&p[offset*2+10], write) ** Perm(&p[offset*2+11], write)
     ** Perm(&p[offset*2+12], write) ** Perm(&p[offset*2+13], write) ** Perm(&p[offset*2+14], write)
     ** Perm(&p[offset*2+15], write);
  ensures data.s0 == p[offset*2] && data.s1 == p[offset*2+1] && data.s2 == p[offset*2+2]
          && data.s3 == p[offset*2+3] && data.s4 == p[offset*2+4] && data.s5 == p[offset*2+5]
          && data.s6 == p[offset*2+6] && data.s7 == p[offset*2+7] && data.s8 == p[offset*2+8]
          && data.s9 == p[offset*2+9] && data.sA == p[offset*2+10] && data.sB == p[offset*2+11]
          && data.sC == p[offset*2+12] && data.sD == p[offset*2+13] && data.sE == p[offset*2+14]
          && data.sF == p[offset*2+15];
@*/
void vstore16(int16 data, int offset, int* p);


#endif

