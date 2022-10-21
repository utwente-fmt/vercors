// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases UnusedCheckedCatchBad
//:: tools silicon
//:: verdict Error

import java.io.*;

final class NotOk {
    void m2() {
        try {

        } catch (IOException e) {

        }
    }
}
