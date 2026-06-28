//:: cases TC_PST_12_JavaEnsuresSatisfiable
//:: tools silicon
//:: verdict Pass

// Same as TC-PST-1 but via Java.
public class TC_PST_12_JavaEnsuresSatisfiable {
    //@ ensures \result > 0;
    public static int f(int x) {
        return 1;
    }
}
