// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Increment
//:: tools silicon

class Incr {

  /*
    @ ensures \result==\old(x)+1;
  */
  public int incr(int x){
    return x+1;
  }

  int y;

  /*
    @ context Perm(y, write);
    @ ensures y == \old(y)+1;
  */
  public void incry(){
    y = y + 1;
  }
}
