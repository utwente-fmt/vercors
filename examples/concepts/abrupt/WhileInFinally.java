// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases WhileInFinally
//:: tools silicon
//:: verdict Pass

import java.io.*;

class C {
    int x;

    //@ requires Perm(x, write) ** x == 0;
    //@ signals (IOException e) Perm(x, write) ** x == 5;
    //@ ensures false;
    void m() throws IOException {
        try {
            throw new IOException();
        } finally {
            //@ loop_invariant Perm(x, write);
            //@ loop_invariant 0 <= x && x <= 5;
            while (x < 5) {
                x += 1;
            }
        }
        x = 0;
    }
}
