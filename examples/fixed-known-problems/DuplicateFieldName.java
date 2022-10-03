// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases DuplicateFieldName
//:: tools silicon
//:: verdict Pass

// Tracked at https://github.com/utwente-fmt/vercors/issues/4

public class A {
  int i;

  //@ requires Perm(i, write);
  public void main() {
    B b = new B();
    i = 3;
    //@ assert i == 3;

    b.testA()/*@ with { a = 9; } @*/;
    b.testI()/*@ with { i = 9; } @*/;
    //@ assert i == 3;
  }
}

class B {
  /*@
    given int a;
  @*/
  public void testA() {
  }

  /*@
    given int i;
    requires i == 9;
  @*/
  public void testI() {
  }
}
