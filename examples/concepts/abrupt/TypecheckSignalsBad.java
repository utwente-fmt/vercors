// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases TypecheckSignalsBad
//:: tools silicon
//:: verdict Error

final class NotOk {
    //@ signals (RuntimeException e) e > 3;
    void m2();
}

