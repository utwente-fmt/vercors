// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Refute5
//:: tools silicon
//:: verdict Fail

/*
 The refute statement is not reachable, because 
 an error occurs before it.
*/
public class Refute {

  int x;

  /*@
    requires Perm(x,write) ** x==2;
    ensures  Perm(x,write) ** x==3;
  @*/
  public void good2(){
    //@ [/expect assertFailed:false] assert x==3; [/end]
    //@ [/expect refuteFailed] refute x==3; [/end]
  }

}

