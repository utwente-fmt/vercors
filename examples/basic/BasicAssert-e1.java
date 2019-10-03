// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases BasicAssert-E1
//:: tools silicon
//:: verdict Fail
class Ref {

  boolean test;

  /*@
    requires b ** Perm(r.test,1f/2);
  @*/
  void t1(boolean b, int d,Ref r)
  {
    r.test = b;
    //@ assert b == (r.test);
  }

}
