// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases ContinueBreak
//:: tools silicon
//:: verdict Pass

class CB {
    boolean p();

    void foo() {
        boolean pp = p();
        while (pp) {
            continue;
            break;
        }
    }
}