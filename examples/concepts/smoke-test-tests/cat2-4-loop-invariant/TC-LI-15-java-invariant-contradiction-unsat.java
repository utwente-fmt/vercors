//:: cases TC_LI_15_JavaInvariantContradictionUnsat
//:: tools silicon
//:: verdict Fail

// Same as TC-LI-13 but via Java.
class TC_LI_15_JavaInvariantContradictionUnsat {
    //@ requires n >= 0;
    void f(int n) {
        int i = 0;
        //@ loop_invariant i >= 0 && i < 0;
        while (i < n) { i = i + 1;}
    }
}
