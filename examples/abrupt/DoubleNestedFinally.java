// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases DoubleNestedFinally.java
//:: tools silicon
//:: verdict Pass

import java.io.*;

class C {
    void m() {
        int x = 0;
        try {
            try {
                throw new IOException();
            } finally {
                try {

                } finally {

                }
                x = 3;
            }
        } catch (IOException e) {
            x += 10;
        }
        assert x == 13;
    }
}
