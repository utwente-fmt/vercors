// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases ContinueBreak
//:: tools silicon
//:: verdict Pass

class CB {
    boolean p();

    void foo() {
        while (p()) {
            continue;
            break;
        }
    }
}