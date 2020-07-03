// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases PureThrowsBad
//:: tools silicon
//:: verdict Error

final class C {
    /*@ pure @*/ int m1() throws Throwable {
        return 1;
    }
}
