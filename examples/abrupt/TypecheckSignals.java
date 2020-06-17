// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases TypecheckSignals
//:: tools silicon
//:: verdict Pass Ok.m1
//:: verdict Fail NotOk.m2 NotOk.m3

final class Ok {
    //@ signals (RuntimeException e) true;
    void m1();
}

final class NotOk1 {
    //@ signals (RuntimeException e) e > 3;
    void m2();

    //@ signals (int e) e > 0;
    void m3();
}

