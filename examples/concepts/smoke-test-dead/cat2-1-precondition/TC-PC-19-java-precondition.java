//:: cases TC_PC_19_JavaPrecondition
//:: tools silicon
//:: verdict Fail

// TC-PC-19: TC-PC-2 repeated in Java — precondition contradiction from Java frontend.
// Verifies the COL transformation for Java annotations handles this correctly.
public class TC_PC_19_JavaPrecondition {
    //@ requires x > 0;
    public static void f(int x) {
        if (x < 0) {
            //@ assert false; // dead: x > 0 AND x < 0 unsatisfiable
        }
    }
}
