// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Refute3
//:: tools silicon
//:: verdict Fail

/*
 The refute statement is reachable, therefore it is valid.
 However, the post-conditions is wrong.
*/
public class Refute {

  int x;
  /*@
    requires Perm(x,write) ** x==2;
    ensures  Perm(x,write) ** x==3;
  @*/
  public void bad2(){
    //@ refute x==3;
  }

}

