// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases BasicAssert
//:: tools silicon
//:: verdict Pass
class Ref {

  boolean test;

  /*@
    requires b ** Perm(r.test,write);
  @*/
  void t1(boolean b, int d,Ref r)
  {
    r.test = b;
    //@ assert b == (r.test);
  }

}
