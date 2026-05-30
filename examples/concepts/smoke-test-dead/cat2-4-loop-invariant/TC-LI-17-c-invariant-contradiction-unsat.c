//:: cases TC_LI_17_CInvariantContradictionUnsat
//:: tools silicon
//:: verdict Fail

// TC-LI-17 (C): loop_invariant i > 0 && i < 0 is a contradiction — no integer satisfies it.
// The sat check fires invariantUnsatisfiable.
// Verifies the invSat checker works correctly through the C frontend.
void f(int n) {
    int i = 0;
    //@ loop_invariant i > 0 && i < 0;
    while (i < n) { i = i + 1; }
}
