//:: cases TC_PC_8_NegativeConsistent
//:: tools silicon
//:: verdict Pass

// TC-PC-8 (Negative): Precondition is consistent with both branches.
// requires x > 0 allows x in {1,2,...} — both x == 1 and x != 1 are possible.
// No dead code warning expected on either branch.
class TC_PC_8_NegativeConsistent {
    //@ requires x > 0;
    void f(int x) {
        if (x == 1) {
            x = x + 1; // live: x could be 1
        } else {
            x = x + 1; // live: x could be 2, 3, ...
        }
    }
}
