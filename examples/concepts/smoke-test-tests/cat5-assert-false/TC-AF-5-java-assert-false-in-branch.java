//:: cases TC_AF_5_JavaAssertFalseInBranch
//:: tools silicon
//:: verdict Fail

// Same as TC-AF-1 but via Java, plus a second, unrelated if (x > 0) afterward.
// The assert false also makes Silicon stop checking the rest of THIS path, so
// the only path reaching the second if is the one where the first condition
// was false (x <= 0) — making it look dead there too. Two errors fire:
// assertFailed, and a misleading deadBranch on the second if, which is
// genuinely reachable in real execution (e.g. starting from x = 3).
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
            x = x - 1;        // misleadingly reported dead — see comment above
        }
    }
}
