//:: cases TC_AF_6_JavaAssumeFalseGivesWarning
//:: tools silicon
//:: verdict Fail

// assume false emits a DeadBranch warning without cutting off the block; the
// following assignment is instrumented but triggers no further checks.
class TC_AF_6_JavaAssumeFalseGivesWarning {
    void f(int x) {
        x = x + 1;
        //@ assume false;      // DeadBranch warning emitted
        x = x + 2;             // assignment — no Refute check placed
    }
}
