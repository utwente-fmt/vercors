//:: cases TC_AF_6_JavaAssumeFalseGivesWarning
//:: tools silicon
//:: verdict Fail

// TC-AF-6 (Java): assume false is no longer a silent dead code marker — DetectDeadCode
// emits a DeadBranch warning at rewrite time. No block cutoff for assume false:
// code after it is still instrumented. Only an assignment follows here, so no
// further Refute checks fire.
//
// Expected errors: 1 (deadBranch warning for assume false).
class TC_AF_6_JavaAssumeFalseGivesWarning {
    void f(int x) {
        x = x + 1;
        //@ assume false;      // DeadBranch warning emitted
        x = x + 2;             // assignment — no Refute check placed
    }
}
