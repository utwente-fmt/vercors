// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases BadLabeledContract
//:: tools silicon
//:: verdict Error

class C {
    void m() {
        //@ loop_invariant true;
        l: if (true) { }
    }
}
