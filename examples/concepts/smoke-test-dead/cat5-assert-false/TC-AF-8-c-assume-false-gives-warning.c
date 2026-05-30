//:: cases TC_AF_8_CAssumeFalseGivesWarning
//:: tools silicon
//:: verdict Fail

// TC-AF-8 (C): assume false is no longer a silent dead code marker — DetectDeadCode
// emits a DeadBranch warning at rewrite time. No block cutoff for assume false:
// code after it is still instrumented. The if (x > 0) after assume false gets a
// Refute(false) check which fires because the state is ⊥ after inhale false.
//
// Expected errors: 2 (deadBranch for assume false + deadBranch for then-branch).
void f(int x) {
    x = x + 1;
    //@ assume false;      // DeadBranch warning emitted
    if (x > 0) {           // instrumented — Refute(false) fires (state is ⊥)
        x = x - 1;
    }
}
