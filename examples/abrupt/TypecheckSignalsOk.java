// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases TypecheckSignalsOk
//:: tools silicon
//:: verdict Pass

import java.io.*;

final class Ok {
    //@ signals (RuntimeException e) true;
    void m1();

    int x;

    void m2() {
        try {
            m3();
        } catch (RuntimeException e) {
            assert x == 3;
        }
    }

    //@ signals (RuntimeException e) Perm(x, read) ** x == 3;
    void m3();

    //@ signals (RuntimeException e) false;
    void m4();

    void m5() {
        m4();
    }
}

