// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Refute1
//:: tools chalice silicon
//:: verdict Pass

/*
 The refute statement is reachable, therefore it is valid.
*/
public class Refute {

  /*@
    requires true;
  @*/
  public void good(){
    //@ refute false;
  }

}

