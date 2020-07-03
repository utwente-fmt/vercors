// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases TypecheckCatch
//:: tools silicon
//:: verdict Error

final class Ok {
    void m1() {
        try {

        } catch (RuntimeException e) {

        }
    }
}

final class NotOk {
    void m2() {
        try {

        } catch (Exception e) {

        }
    }
}
