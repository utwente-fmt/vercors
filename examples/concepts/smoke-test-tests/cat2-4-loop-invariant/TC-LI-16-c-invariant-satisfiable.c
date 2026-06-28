//:: cases TC_LI_16_CInvariantSatisfiable
//:: tools silicon
//:: verdict Pass

// Same as TC-LI-11 but via C.
/*@ requires n >= 0; @*/
void f(int n) {
    int i = 0;
    //@ loop_invariant i >= 0;
    while (i < n) { i = i + 1; }
}
