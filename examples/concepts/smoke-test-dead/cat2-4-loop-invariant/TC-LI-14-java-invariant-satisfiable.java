//:: cases TC_LI_14_JavaInvariantSatisfiable
//:: tools silicon
//:: verdict Pass

// TC-LI-14 (Java): loop_invariant i >= 0 is satisfiable — i = 0 witnesses it.
// The invariant satisfiability check must NOT fire.
// Verifies the invSat checker works correctly through the Java frontend.
class TC_LI_14_JavaInvariantSatisfiable {
    //@ requires n >= 0;
    void f(int n) {
        int i = 0;
        //@ loop_invariant i >= 0;
        while (i < n) { }
    }
}
