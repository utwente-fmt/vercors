// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases TypecheckThrowsMethodBad
//:: tools silicon
//:: verdict Error

import java.io.*;

class C {
    void m1() {
        m2();
    }

    void m2() throws IOException;
}