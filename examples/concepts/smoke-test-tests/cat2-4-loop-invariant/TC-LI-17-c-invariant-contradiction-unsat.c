//:: cases TC_LI_17_CInvariantContradictionUnsat
//:: tools silicon
//:: verdict Fail

// Same as TC-LI-13 but via C.
void f(int n) {
    int i = 0;
    //@ loop_invariant i > 0 && i < 0;
    while (i < n) { i = i + 1; }
}
