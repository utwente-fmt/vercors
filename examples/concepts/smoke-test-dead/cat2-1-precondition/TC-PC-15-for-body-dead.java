//:: cases TC_PC_15_ForBodyDead
//:: tools silicon
//:: verdict Fail

// TC-PC-15: For loop body is dead because the initial condition is never true.
// requires x > 10 means i starts > 10, so i < 0 is immediately false.
class TC_PC_15_ForBodyDead {
    //@ requires x > 10;
    void f(int x) {
        //@ loop_invariant i >= 0;
        for (int i = x; i < 0; i = i + 1) {
            x = x + 1; // dead: i starts > 10, condition i < 0 never true
        }
    }
}
