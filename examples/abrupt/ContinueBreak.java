// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases ContinueBreak
//:: tools silicon
//:: verdict Pass CB.m1
//:: verdict Fail CB.m2

class CB {
    boolean p();

    void m1() {
        boolean pp = p();
        while (pp) {
            continue;
            assert false;
        }
    }

    void m2() {
        boolean pp = p();
        while (pp) {
            break;
        }
        assert false; // Should be triggered
    }
}