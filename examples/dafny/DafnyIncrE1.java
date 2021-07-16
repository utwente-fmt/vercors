// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases IncrementError
//:: tools silicon
//:: verdict Fail
class Incr {

    int y;

    /*@
      ensures \result==\old(x)+1;
    @*/
    public int incr(int x) {
        return x + 1;
    }

    /*@
      requires true;
      ensures y == \old(y)+1;
    @*/
    public void incry() {
        y = y + 1;
    }
}
