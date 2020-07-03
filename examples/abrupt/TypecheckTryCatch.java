// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases TypecheckTryCatch
//:: tools silicon
//:: verdict Pass

import java.io.*;

class C1 {
    void m1() {
        try {

        } catch (RuntimeException e) {

        }
    }
}

class C2 {
    void m1() {
        try {
            throw new IOException();
        } catch (IOException e) {

        }
    }
}

class C2 {
    void m1() {
        try {
            m2();
        } catch (IOException e) {

        }
    }

    void m2() throws IOException;
}
