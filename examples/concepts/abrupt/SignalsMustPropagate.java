// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases SignalsMustPropagate
//:: tools silicon
//:: verdict Fail

class C {
    /*[/expect extraExc]*/
    void m1() {
        m2();
    }
    /*[/end]*/

    //@ signals (RuntimeException e) true;
    void m2();
}
