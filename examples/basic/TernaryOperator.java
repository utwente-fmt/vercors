// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: case TernaryOperator
//:: tools silicon
//:: verdict Pass

class Test {
    void usage(boolean cond) {
        int result = cond ? expectEqual(cond, true) : expectEqual(cond, false);
    }

    /*@
    requires a == b;
    @*/
    int expectEqual(boolean a, boolean b) {
        return 0;
    }
}