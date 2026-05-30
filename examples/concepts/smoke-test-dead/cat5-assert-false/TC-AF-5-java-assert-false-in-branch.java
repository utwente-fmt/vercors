//:: cases TC_AF_5_JavaAssertFalseInBranch
//:: tools silicon
//:: verdict Fail

// TC-AF-5 (Java): assert false as the first statement of a live branch body.
// Block cutoff applies within that branch body: the nested if (x < 5) and the
// assignment after assert false are NOT instrumented. The second outer if (x > 0)
// is outside the cutoff block and is still instrumented (check passes).
// The assert false itself fails (assertFailed).
//
// Expected errors: 1 (assertFailed from assert false).
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
