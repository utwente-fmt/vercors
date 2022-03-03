// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: case OpenCLincr
// tools silicon

#include <opencl.h>

/*@
  context_everywhere c!=NULL;
  requires Perm(c[get_global_id(0)],write);
  ensures  PointsTo(c[get_global_id(0)],write,\old(c[get_global_id(0)])+1);
@*/
__kernel void simpleAdd(int veclen, int c[])
{
  // get the index of the test we are performing
  int index = get_global_id(0);
 
  c[index] = c[index] + 1;
}
