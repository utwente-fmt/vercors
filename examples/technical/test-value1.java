// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases TestValue1
//:: tools silicon
//:: verdict Fail

class Test {

  Test next;
  
  /*@
    requires Value(t1.next) ** Value(t2.next);
    requires t1.next != null ** t2.next != null;
  @*/
  void main1(Test t1, Test t2){
    if (t1==t2){
      //@ assert false;
    }
  }

}
