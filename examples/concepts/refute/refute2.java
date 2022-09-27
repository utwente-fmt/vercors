// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Refute2
// tools silicon
// verdict Fail

/*
 The refute statement is not reachable, therefore it is invalid.
*/
public class Refute {

  /*@
    requires true;
  @*/
  public void bad(){
    if (false) {
      //@ [/expect refuteFailed] refute false; [/end]
    }
  }

}

