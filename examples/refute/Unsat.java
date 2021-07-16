// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases RefuteUnsat
//:: tools silicon
//:: verdict Fail

/*
  The pre-condition is unsatisfiable.
*/
public class Unsat {

    /*@
      requires 1==0;
    @*/
    public void bad() {
    }

}

