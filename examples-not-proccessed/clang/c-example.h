// -*- tab-width:2 ; indent-tabs-mode:nil -*-

#ifndef _C_EXAMPLE_H
#define _C_EXAMPLE_H

extern int x;

/*@
  requires Perm(x,1);
  ensures Perm(x,1) ** x==\old(x)+1;
@*/
extern void test();

#endif


