// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases ghi
//:: suite ContractSatisfiable
//:: tools silicon
//:: verdict Pass MyClass.foo
//:: verdict Fail MyClass.baz MyClass.bar MyClass.qux MyClass.complicated_arguments

class MyClass {
  // User indicates no checking is desired for this function
  //@ requires false;
  void foo() {
    // Should not be triggered
    //@ assert 1 == 2;
  }

  // User makes a mistake here
  //@ requires 3 == 4;
  void bar() {
    // Will not be detected but once the requires is fixed should be
    //@ assert 5 == 6;
  }

  // Perfectly fine
  //@ requires 7 == 7;
  void baz() {
    // Obviously would be detected
    //@ assert 8 == 9;
  }

  // Should not change the situation
  void qux() {
    // Obviously would be detected
    //@ assert 10 == 11;
  }

  // User error, should be detected
  requires a > 0 && a < 0 && b > 10;
  void complicated_arguments(int a, int b) {
    // Should not be detected
    //@ assert b < 10;
  }
}
