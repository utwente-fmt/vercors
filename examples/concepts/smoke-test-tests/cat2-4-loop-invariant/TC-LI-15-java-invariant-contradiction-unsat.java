//:: cases TC_LI_15_JavaInvariantContradictionUnsat
//:: tools silicon
//:: verdict Fail

// TC-LI-15 (Java): loop_invariant i > 0 && i < 0 is a contradiction — no integer satisfies it.
// The sat check fires invariantUnsatisfiable.
// Verifies the invSat checker works correctly through the Java frontend.
class TC_LI_15_JavaInvariantContradictionUnsat {
    //@ requires n >= 0;
    void f(int n) {
        int i = 0;
        //@ loop_invariant i >= 0 && i < 0;
        while (i < n) { i = i + 1;}
    }
}
