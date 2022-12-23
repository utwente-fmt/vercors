// -*- tab-width:2 ; indent-tabs-mode:nil -*-

//:: cases CExampleUse
//:: tools silicon
//:: verdict Pass

#include "c-example.h"

/*@
  requires Perm(x,1);
  ensures Perm(x,1) ** x==4;
@*/
void use(){
  x=3;
  test();
}

