// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases IncrementError
//:: tools silicon
//:: verdict Fail
class Incr {

  /*@
    ensures \result==\old(x)+1;
  @*/
  public int incr(int x){
    return x+1;
  }

  int y;

  /*@
    requires true;
    ensures [/expect perm]y[/end] == \old(y)+1;
  @*/
  public void incry(){
    y = y + 1;
  }
}
