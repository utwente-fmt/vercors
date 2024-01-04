// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Overloading
//:: tools silicon
//:: verdict Pass

//@ ensures \result == 1;
int method(int number);

//@ ensures \result == 2;
int method(float number);

//@ ensures \result == 3;
int method(int number, int number2);

//@ ensures \result == 4;
int method(int number, int number2);

void test() {
  //@ assert method(1) == 1;
  //@ assert method(1.0f) == 2;
  //@ assert method(1, 2) == 4;
}