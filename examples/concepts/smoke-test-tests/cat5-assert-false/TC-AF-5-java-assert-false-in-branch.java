//:: cases TC_AF_5_JavaAssertFalseInBranch
//:: tools silicon
//:: verdict Fail

// Same as TC-AF-1 but via Java.
class TC_AF_5_JavaAssertFalseInBranch {
    void f(int x) {
        if (x > 0) {
            //@ assert false;   // assertFailed — block cutoff within this branch body
            if (x < 5) {       // not instrumented (block cutoff)
                x = x + 1;
            }
            x = x - 1;
        }
        if (x > 0) {
            x = x - 1;        // instrumented — check passes (x > 0 is satisfiable)
        }
    }
}
