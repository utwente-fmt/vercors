// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases TypecheckCatch
//:: tools silicon
//:: verdict Pass Ok.m1
//:: verdict Fail NotOk.m2

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
