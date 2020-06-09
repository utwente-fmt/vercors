// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases OnlyReturn
//:: tools silicon
//:: verdict Pass

final class MyClas {
    //@ ensures !\result;
    final boolean foo() {
        return false;
    }

    //@ ensures \result;
    final boolean bar() {
        return true;
    }

    //@ ensures \result == 5;
    final int baz() {
        return 5;
    }

    final void qux() {
        return;
    }
}
