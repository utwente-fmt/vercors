//:: cases TC_PST_12_JavaEnsuresSatisfiable
//:: tools silicon
//:: verdict Pass

// TC-PST-12 (Java): Postcondition \result > 0 is satisfiable on its own (e.g., result=1).
// Verifies the postSat checker works correctly through the Java frontend.
public class TC_PST_12_JavaEnsuresSatisfiable {
    //@ ensures \result > 0;
    public static int f(int x) {
        return 1;
    }
}
