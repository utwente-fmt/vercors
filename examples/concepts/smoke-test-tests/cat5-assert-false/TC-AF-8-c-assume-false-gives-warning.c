//:: cases TC_AF_8_CAssumeFalseGivesWarning
//:: tools silicon
//:: verdict Fail

// assume false emits a DeadBranch warning but does not cut off the block, so the
// if (x > 0) after it is still instrumented and also reports dead (2 errors total).
void f(int x) {
    x = x + 1;
    // DeadBranch warning emitted
    //@ assume false;
    if (x > 0) {           // instrumented — Refute(false) fires (state is ⊥)
        x = x - 1;
    }
}
