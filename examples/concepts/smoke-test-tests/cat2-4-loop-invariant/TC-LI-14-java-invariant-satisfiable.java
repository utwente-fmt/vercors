//:: cases TC_LI_14_JavaInvariantSatisfiable
//:: tools silicon
//:: verdict Pass

// Same as TC-LI-11 but via Java.
class TC_LI_14_JavaInvariantSatisfiable {
    //@ requires n >= 0;
    void f(int n) {
        int i = 0;
        //@ loop_invariant i >= 0;
        while (i < n) { }
    }
}
