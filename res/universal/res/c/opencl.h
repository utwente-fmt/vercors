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

#endif

