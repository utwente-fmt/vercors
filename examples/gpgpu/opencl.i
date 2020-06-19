# 1 "opencl.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 341 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "opencl.c" 2
//:: cases BasicOpenCL
//:: verdict Pass


# 1 "/home/pieter/vercors/src/main/universal/res/include/opencl.h" 1 3








extern /*@ pure @*/ int get_work_dim(); // Number of dimensions in use

extern /*@ pure @*/ int get_global_size(int dimindx); // Number of global work-items  

extern /*@ pure @*/ int get_global_id (int dimindx); // Global work-item ID value

extern /*@ pure @*/ int get_local_size (int dimindx); //

extern /*@ pure @*/ int get_enqueued_local_size (int dimindx); // Number of local work-items 

extern /*@ pure @*/ int get_local_id (int dimindx); // Local work-item ID 

extern /*@ pure @*/ int get_num_groups (int dimindx); // Number of work-groups

extern /*@ pure @*/ int get_group_id (int dimindx); // Work-group ID 

extern /*@ pure @*/ int get_global_offset (int dimindx); // Global offset

extern /*@ pure @*/ int get_global_linear_id (); // Work-items, 1-dimensional global ID

extern /*@ pure @*/ int get_local_linear_id (); // Work-items, 1-dimensional local ID

extern /*@ pure @*/ int get_sub_group_size (); // Number of work-items in the subgroup

extern /*@ pure @*/ int get_max_sub_group_size (); // Maximum size of a subgroup

extern /*@ pure @*/ int get_num_sub_groups (); // Number of subgroups

extern /*@ pure @*/ int get_enqueued_num_sub_groups (); //

extern /*@ pure @*/ int get_sub_group_id (); // Sub-group ID

extern /*@ pure @*/ int get_sub_group_local_id (); // Unique work-item ID
# 5 "opencl.c" 2

/*@
    context_everywhere opencl_gcount == 1;
    context \pointer_index(a, get_local_id(0)*2, write);
    context \pointer_index(a, get_local_id(0)*2+1, write);
    context \pointer_index(b, get_local_id(0), write);
@*/
__kernel void example(int a[], int b[], int len) {
    int tid = get_local_id(0);
    a[tid*2] = tid;
    /*@
        context \pointer_index(a, get_local_id(0)*2, write);
        context \pointer_index(a, get_local_id(0)*2+1, write);
    @*/
    __vercors_barrier__(__vercors_local_barrier__);
    a[tid*2+1] = a[tid*2] * 2;
}
