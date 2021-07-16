// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases TypeExample1
//:: tools silicon
//:: verdict Pass

/*
  This file show how the syntax for stating that an object 
  is an instance of a specific class (\typeof) works.
  It is future work to make sure that the knowledge
  that this is an instance of C is automatically added.
*/

class C {

    //@ requires \typeof(this) == C;
    void m1() {
        //@ assert this instanceof C;
        //@ assert \typeof(this) == C;
    }

    //@ requires this instanceof C;
    void m2() {
        //@ assert this instanceof C;
    }

}
