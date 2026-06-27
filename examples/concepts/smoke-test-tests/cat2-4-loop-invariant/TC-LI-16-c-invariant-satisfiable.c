//:: cases TC_LI_16_CInvariantSatisfiable
//:: tools silicon
//:: verdict Pass

// TC-LI-16 (C): loop_invariant i >= 0 is satisfiable — i = 0 witnesses it.
// The invariant satisfiability check must NOT fire.
// Verifies the invSat checker works correctly through the C frontend.
/*@ requires n >= 0; @*/
void f(int n) {
    int i = 0;
    //@ loop_invariant i >= 0;
    while (i < n) { i = i + 1; }
}
