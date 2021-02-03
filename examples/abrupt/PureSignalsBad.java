// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases PureSignalsBad
//:: tools silicon
//:: verdict Error

final class C {
    //@ signals (RuntimeException e) true;
    /*@ pure @*/ int m2() {
        return 2;
    }
}
