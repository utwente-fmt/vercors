// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases NestedTryCatchFinally
//:: tools silicon
//:: verdict Pass

import java.io.*;

class C {
    //@ signals (IOException e) true;
    //@ ensures false;
    void m() throws IOException {
        try {
            throw new IOException();
        } finally {
            try {
                throw new ArithmeticException();
            } catch (ArithmeticException e) {

            }
        }
    }
}
