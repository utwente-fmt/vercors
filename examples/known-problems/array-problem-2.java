// -*- tab-width:2 ; indent-tabs-mode:nil -*-

//:: cases ArrayProblem2Full
//:: suite problem-fail
//:: tools silicon
//:: verdict Fail test1 test2

class C {

  int F;
  
  C[] array;

  /*@
    requires Value(array);
    requires array!=null;
    requires (\forall* int i ; 0 <= i && i < array.length;Value(array[i]));
    requires (\forall* int i ; 0 <= i && i < array.length;Value(array[i].F));
    requires (\forall int i ; 0 <= i && i < array.length; array[i].F > 0 );
  @*/
  void test1(){
  }
  
  /*@
    requires Value(array);
    requires array!=null;
    requires (\forall* int i ; 0 <= i && i < array.length;Perm(array[i],1\100));
    requires (\forall* int i ; 0 <= i && i < array.length;Perm(array[i].F,1\100));
    requires (\forall int i ; 0 <= i && i < array.length; array[i].F > 0 );
  @*/
  void test2(){
  }
  
  public C(){
    //@ assume false;
  }
  
}
