// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases ThrowIntBad
//:: tools silicon
//:: verdict Error

class C {
    void foo() {
        throw 3;
    }
}
