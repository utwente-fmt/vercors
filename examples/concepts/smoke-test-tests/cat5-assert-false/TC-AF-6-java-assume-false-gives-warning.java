//:: cases TC_AF_6_JavaAssumeFalseGivesWarning
//:: tools silicon
//:: verdict Fail

// Same as TC-AF-3 but via Java.
class TC_AF_6_JavaAssumeFalseGivesWarning {
    void f(int x) {
        x = x + 1;
        //@ assume false;      // DeadBranch warning emitted
        x = x + 2;             // assignment — no Refute check placed
    }
}
