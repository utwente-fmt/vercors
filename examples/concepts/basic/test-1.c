// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Test-1-c
//:: tools silicon
//:: verdict Pass

// Simple method contract example.
/*@
  requires y>=0;
  ensures  \result > 0;
@*/
int m(int y){
  int x=1;
  y=y+x;
  return y;
}
