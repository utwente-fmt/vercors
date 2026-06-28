//:: cases TC_AF_8_CAssumeFalseGivesWarning
//:: tools silicon
//:: verdict Fail

// Same as TC-AF-3 but via C.
void f(int x) {
    x = x + 1;
    // DeadBranch warning emitted
    //@ assume false;
    if (x > 0) {           // instrumented — Refute(false) fires (state is ⊥)
        x = x - 1;
    }
}
