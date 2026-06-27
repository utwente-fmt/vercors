//:: cases TC_PST_13_JavaEnsuresUnsat
//:: tools silicon
//:: verdict Fail

// TC-PST-13 (Java): \result > 0 && \result < 0 is always false — no value satisfies both.
// Triggers both a normal postcondition failure and postUnsatisfiable.
// Verifies the postSat checker works correctly through the Java frontend.
public class TC_PST_13_JavaEnsuresUnsat {
    //@ ensures \result > 0 && \result < 0;
    public static int f(int x) {
        return 1;
    }
}
