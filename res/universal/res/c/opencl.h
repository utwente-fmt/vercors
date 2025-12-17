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
  context p != NULL;
  context offset >= 0;
  context \pointer_length(p) >= offset*2+2;
  requires Perm(p[offset*2], read) ** Perm(p[offset*2+1], read);
  ensures \result.s0 == p[offset*2] && \result.s1 == p[offset*2+1];
@*/
/*@ pure */ int2 vload2(int offset, int* p);

/*@
  context p != NULL;
  context offset >= 0;
  context \pointer_length(p) >= offset*3+3;
  requires Perm(p[offset*3], read) ** Perm(p[offset*3+1], read) ** Perm(p[offset*3+2], read);
  ensures \result.s0 == p[offset*3] && \result.s1 == p[offset*3+1] && \result.s2 == p[offset*3+2];
@*/
/*@ pure */ int3 vload3(int offset, int* p);

/*@
  context p != NULL;
  context offset >= 0;
  context \pointer_length(p) >= offset*4+4;
  requires Perm(p[offset*4], read) ** Perm(p[offset*4+1], read) ** Perm(p[offset*4+2], read) ** Perm(p[offset*4+3], read);
  ensures \result.s0 == p[offset*4] && \result.s1 == p[offset*4+1] && \result.s2 == p[offset*4+2]
      && \result.s3 == p[offset*4+3];
@*/
/*@ pure */ int4 vload4(int offset, int* p);

/*@
  context p != NULL;
  context offset >= 0;
  context \pointer_length(p) >= offset*8+8;
  requires Perm(p[offset*8], read) ** Perm(p[offset*8+1], read) ** Perm(p[offset*8+2], read) ** Perm(p[offset*8+3], read)
     ** Perm(p[offset*8+4], read) ** Perm(p[offset*8+5], read) ** Perm(p[offset*8+6], read) ** Perm(p[offset*8+7], read);
  ensures \result.s0 == p[offset*8] && \result.s1 == p[offset*8+1] && \result.s2 == p[offset*8+2]
      && \result.s3 == p[offset*8+3] && \result.s4 == p[offset*8+4] && \result.s5 == p[offset*8+5]
      && \result.s6 == p[offset*8+6] && \result.s7 == p[offset*8+7];
@*/
/*@ pure */ int8 vload8(int offset, int* p);

/*@
  context p != NULL;
  context offset >= 0;
  context \pointer_length(p) >= offset*16+16;
  requires Perm(p[offset*16], read) ** Perm(p[offset*16+1], read) ** Perm(p[offset*16+2], read) ** Perm(p[offset*16+3], read)
     ** Perm(p[offset*16+4], read) ** Perm(p[offset*16+5], read) ** Perm(p[offset*16+6], read) ** Perm(p[offset*16+7], read)
     ** Perm(p[offset*16+8], read) ** Perm(p[offset*16+9], read) ** Perm(p[offset*16+10], read) ** Perm(p[offset*16+11], read)
     ** Perm(p[offset*16+12], read) ** Perm(p[offset*16+13], read) ** Perm(p[offset*16+14], read) ** Perm(p[offset*16+15], read);
  ensures \result.s0 == p[offset*16] && \result.s1 == p[offset*16+1] && \result.s2 == p[offset*16+2]
      && \result.s3 == p[offset*16+3] && \result.s4 == p[offset*16+4] && \result.s5 == p[offset*16+5]
      && \result.s6 == p[offset*16+6] && \result.s7 == p[offset*16+7] && \result.s8 == p[offset*16+8]
      && \result.s9 == p[offset*16+9] && \result.sA == p[offset*16+10] && \result.sB == p[offset*16+11]
      && \result.sC == p[offset*16+12] && \result.sD == p[offset*16+13] && \result.sE == p[offset*16+14]
      && \result.sF == p[offset*16+15];
@*/
/*@ pure */ int16 vload16(int offset, int* p);

/*@
  context p != NULL;
  context offset >= 0;
  context \pointer_length(p) >= offset*2+2;
  context Perm(p[offset*2], write) ** Perm(p[offset*2+1], write);
  ensures data.s0 == p[offset*2] && data.s1 == p[offset*2+1];
@*/
void vstore2(int2 data, int offset, int* p);

/*@
  context p != NULL;
  context offset >= 0;
  context \pointer_length(p) >= offset*3+3;
  context Perm(p[offset*3], write) ** Perm(p[offset*3+1], write) ** Perm(p[offset*3+2], write);
  ensures data.s0 == p[offset*3] && data.s1 == p[offset*3+1] && data.s2 == p[offset*3+2];
@*/
void vstore3(int3 data, int offset, int* p);

/*@
  context p != NULL;
  context offset >= 0;
  context \pointer_length(p) >= offset*4+4;
  context Perm(p[offset*4], write) ** Perm(p[offset*4+1], write) ** Perm(p[offset*4+2], write)
     ** Perm(p[offset*4+3], write);
  ensures data.s0 == p[offset*4] && data.s1 == p[offset*4+1] && data.s2 == p[offset*4+2]
          && data.s3 == p[offset*4+3];
@*/
void vstore4(int4 data, int offset, int* p);

/*@
  context p != NULL;
  context offset >= 0;
  context \pointer_length(p) >= offset*8+8;
  context Perm(p[offset*8], write) ** Perm(p[offset*8+1], write) ** Perm(p[offset*8+2], write)
     ** Perm(p[offset*8+3], write) ** Perm(p[offset*8+4], write) ** Perm(p[offset*8+5], write)
     ** Perm(p[offset*8+6], write) ** Perm(p[offset*8+7], write);
  ensures data.s0 == p[offset*8] && data.s1 == p[offset*8+1] && data.s2 == p[offset*8+2]
          && data.s3 == p[offset*8+3] && data.s4 == p[offset*8+4] && data.s5 == p[offset*8+5]
          && data.s6 == p[offset*8+6] && data.s7 == p[offset*8+7];
@*/
void vstore8(int8 data, int offset, int* p);

/*@
  context p != NULL;
  context offset >= 0;
  context \pointer_length(p) >= offset*16+16;
  context Perm(p[offset*16], write) ** Perm(p[offset*16+1], write) ** Perm(p[offset*16+2], write)
     ** Perm(p[offset*16+3], write) ** Perm(p[offset*16+4], write) ** Perm(p[offset*16+5], write)
     ** Perm(p[offset*16+6], write) ** Perm(p[offset*16+7], write) ** Perm(p[offset*16+8], write)
     ** Perm(p[offset*16+9], write) ** Perm(p[offset*16+10], write) ** Perm(p[offset*16+11], write)
     ** Perm(p[offset*16+12], write) ** Perm(p[offset*16+13], write) ** Perm(p[offset*16+14], write)
     ** Perm(p[offset*16+15], write);
  ensures data.s0 == p[offset*16] && data.s1 == p[offset*16+1] && data.s2 == p[offset*16+2]
          && data.s3 == p[offset*16+3] && data.s4 == p[offset*16+4] && data.s5 == p[offset*16+5]
          && data.s6 == p[offset*16+6] && data.s7 == p[offset*16+7] && data.s8 == p[offset*16+8]
          && data.s9 == p[offset*16+9] && data.sA == p[offset*16+10] && data.sB == p[offset*16+11]
          && data.sC == p[offset*16+12] && data.sD == p[offset*16+13] && data.sE == p[offset*16+14]
          && data.sF == p[offset*16+15];
@*/
void vstore16(int16 data, int offset, int* p);


#endif

