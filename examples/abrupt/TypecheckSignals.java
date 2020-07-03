// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases TypecheckSignals
//:: tools silicon
//:: verdict Error

final class Ok {
    //@ signals (RuntimeException e) true;
    void m1();
}

final class NotOk1 {
    //@ signals (RuntimeException e) e > 3;
    void m2();
}

