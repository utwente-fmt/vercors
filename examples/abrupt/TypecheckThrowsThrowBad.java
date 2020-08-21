// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases TypecheckThrowsThrowBad
//:: tools silicon
//:: verdict Error

import java.io.*;

class C {
    void m() {
        throw new IOException();
    }
}